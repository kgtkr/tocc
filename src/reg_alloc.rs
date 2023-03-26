use crate::tac::{Func, Instr};
use std::collections::{HashMap, HashSet};

pub fn reg_alloc(func: &mut Func) {
    let usages = func
        .bbs
        .iter()
        .map(|bb| (bb.id, bb.local_usage()))
        .collect::<HashMap<_, _>>();
    let mut in_ = func
        .bbs
        .iter()
        .map(|bb| (bb.id, HashSet::new()))
        .collect::<HashMap<_, _>>();
    let mut out = func
        .bbs
        .iter()
        .map(|bb| (bb.id, HashSet::new()))
        .collect::<HashMap<_, _>>();
    let mut succs = func
        .bbs
        .iter()
        .map(|bb| (bb.id, HashSet::new()))
        .collect::<HashMap<_, _>>();
    for bb in &func.bbs {
        for next in bb.term().nexts() {
            succs.get_mut(&next).unwrap().insert(bb.id);
        }
    }

    let mut changed = true;
    while changed {
        changed = false;
        // TODO: 半トポロジカルソートして逆順にすると早くなる
        for bb in &func.bbs {
            let prev_in = in_[&bb.id].clone();
            let prev_out = out[&bb.id].clone();

            for succ in &succs[&bb.id] {
                *out.get_mut(&bb.id).unwrap() = out[&bb.id].union(&in_[succ]).copied().collect();
            }

            *in_.get_mut(&bb.id).unwrap() = usages[&bb.id]
                .gen
                .union(
                    &out[&bb.id]
                        .difference(&usages[&bb.id].kill)
                        .copied()
                        .collect(),
                )
                .copied()
                .collect();

            if prev_in != in_[&bb.id] || prev_out != out[&bb.id] {
                changed = true;
            }
        }
    }

    // (bb_idx, instr_idx)
    let mut local_live_first_kills = HashMap::new();
    let mut local_live_last_gens = HashMap::new();
    let mut use_as_ref = HashSet::new();

    for (bb_idx, bb) in func.bbs.iter().enumerate() {
        for inputs in in_.get(&bb.id).unwrap() {
            let point = (bb_idx, 0);
            let cur = local_live_first_kills.get(inputs).unwrap_or(&point);
            local_live_first_kills.insert(*inputs, std::cmp::min(*cur, point));
        }

        for outputs in out.get(&bb.id).unwrap() {
            let point = (bb_idx, bb.instrs.len() - 1);
            let cur = local_live_last_gens.get(outputs).unwrap_or(&point);
            local_live_last_gens.insert(*outputs, std::cmp::max(*cur, point));
        }

        for (instr_idx, instr) in bb.instrs.iter().enumerate() {
            let point = (bb_idx, instr_idx);
            let usage = instr.local_usage();
            for local in &usage.kill {
                let cur = local_live_first_kills.get(local).unwrap_or(&point);
                local_live_first_kills.insert(*local, std::cmp::min(*cur, point));
            }

            for local in &usage.gen {
                let cur = local_live_last_gens.get(local).unwrap_or(&point);
                local_live_last_gens.insert(*local, std::cmp::max(*cur, point));
            }

            for local in &usage.referenced {
                use_as_ref.insert(*local);
            }
        }
    }

    let range = (0..func.locals.len())
        .filter_map(|local_idx| {
            if use_as_ref.contains(&local_idx) {
                None
            } else {
                let first_kill = local_live_first_kills.get(&local_idx);
                let last_gen = local_live_last_gens.get(&local_idx);
                match (first_kill, last_gen) {
                    (Some(&first_kill), Some(&last_gen)) => {
                        assert!(first_kill <= last_gen); // TODO: エラー処理。定義より前に参照していたら未定義動作
                        Some((local_idx, (first_kill, last_gen)))
                    }
                    // 未参照変数や未定義変数の扱いめんどくさいのでいったんspill
                    _ => None,
                }
            }
        })
        .collect::<HashMap<_, _>>();

    const MAX_REGS: usize = 7;
    let mut active = HashSet::new();
    let mut free = (0..MAX_REGS).collect::<HashSet<_>>();
    let locals_sorted_by_start = {
        let mut locals_sorted_by_start = range.iter().collect::<Vec<_>>();
        locals_sorted_by_start.sort_by_key(|&(_, (start, _))| start);
        locals_sorted_by_start
            .into_iter()
            .map(|(&local_idx, _)| local_idx)
            .collect::<Vec<_>>()
    };
    let mut local2reg = HashMap::new();
    for local_idx in locals_sorted_by_start {
        let (local_start, local_end) = range.get(&local_idx).unwrap();

        // expire
        active.retain(|active_local_idx| {
            let (_, active_end) = range.get(active_local_idx).unwrap();
            if active_end < local_start {
                let reg = local2reg.get(active_local_idx).unwrap();
                free.insert(*reg);
                false
            } else {
                true
            }
        });

        if let Some(&reg) = free.iter().next() {
            active.insert(local_idx);
            local2reg.insert(local_idx, reg);
            free.remove(&reg);
        } else {
            // TODO: 計算量
            let spill = active
                .iter()
                .filter(|active_local_idx| {
                    let (_, active_end) = range.get(active_local_idx).unwrap();
                    active_end > local_end
                })
                .max_by_key(|active_local| {
                    let (_, active_end) = range.get(active_local).unwrap();
                    active_end
                });

            if let Some(&spill_local) = spill {
                let reg = local2reg.remove(&spill_local).unwrap();
                active.remove(&spill_local);
                active.insert(local_idx);
                local2reg.insert(local_idx, reg);
            }
        }
    }

    for (local_idx, reg) in &local2reg {
        func.locals[*local_idx].reg = Some(*reg);
    }

    for bb_idx in 0..func.bbs.len() {
        for instr_idx in 0..func.bbs[bb_idx].instrs.len() {
            if let Instr::Call(instr) = &mut func.bbs[bb_idx].instrs[instr_idx] {
                instr.save_regs = (0..func.locals.len())
                    .filter_map(|local_idx| {
                        let is_live = range
                            .get(&local_idx)
                            .map(|(start, end)| {
                                let cur = (bb_idx, instr_idx);
                                start <= &cur && &cur <= end
                            })
                            .unwrap_or(false);
                        if is_live {
                            func.locals[local_idx].reg
                        } else {
                            None
                        }
                    })
                    .collect();
            }
        }
    }
}
