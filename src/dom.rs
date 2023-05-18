use std::collections::{HashMap, HashSet};

use crate::tac::{BBId, Func};

fn dom(func: &Func) -> HashMap<BBId, HashSet<BBId>> {
    // TODO: reg_allocと重複
    let mut preds = func
        .bbs
        .iter()
        .map(|bb| (bb.id, HashSet::new()))
        .collect::<HashMap<_, _>>();
    for bb in &func.bbs {
        for next in bb.term().nexts() {
            preds.get_mut(&next).unwrap().insert(bb.id);
        }
    }

    let mut doms = func
        .bbs
        .iter()
        .map(|bb| {
            (bb.id, {
                if bb.id == func.entry {
                    HashSet::from([bb.id])
                } else {
                    func.bbs.iter().map(|bb| bb.id).collect()
                }
            })
        })
        .collect::<HashMap<_, _>>();
    let mut changed = true;

    while changed {
        changed = false;
        for bb in func.bbs.iter().filter(|bb| bb.id != func.entry) {
            let mut new_dom = preds[&bb.id]
                .iter()
                .map(|pred| doms[pred].clone())
                .fold::<Option<HashSet<BBId>>, _>(None, |acc, dom| {
                    if let Some(acc) = acc {
                        Some(acc.intersection(&dom).cloned().collect())
                    } else {
                        Some(dom)
                    }
                })
                .unwrap();
            new_dom.insert(bb.id);
            if new_dom.len() != doms[&bb.id].len() {
                doms.insert(bb.id, new_dom);
                changed = true;
            }
        }
    }

    doms
}

fn sdom(func: &Func) -> HashMap<BBId, HashSet<BBId>> {
    let doms = dom(func);
    let sdoms = doms
        .iter()
        .map(|(a, dom)| {
            (
                *a,
                dom.into_iter()
                    .filter(|&b| a != b)
                    .copied()
                    .collect::<HashSet<_>>(),
            )
        })
        .collect::<HashMap<_, _>>();
    sdoms
}

fn idom(sdoms: HashMap<BBId, HashSet<BBId>>) -> HashMap<BBId, HashSet<BBId>> {
    let mut sdom_preds = sdoms
        .iter()
        .map(|(bb_id, _)| (bb_id, HashSet::new()))
        .collect::<HashMap<_, _>>();
    for (a, sdom) in &sdoms {
        for b in sdom {
            sdom_preds.get_mut(b).unwrap().insert(*a);
        }
    }

    let idoms = sdoms
        .iter()
        .map(|(a, sdom)| {
            (*a, {
                sdom.iter()
                    .filter(|b| sdom_preds[b].iter().all(|c| !sdom.contains(c)))
                    .copied()
                    .collect::<HashSet<_>>()
            })
        })
        .collect::<HashMap<_, _>>();

    idoms
}

fn dom_front(func: &Func) -> HashMap<BBId, HashSet<BBId>> {
    let sdom = sdom(func);
    let idom = idom(sdom.clone());

    let mut df = func
        .bbs
        .iter()
        .map(|bb| (bb.id, HashSet::new()))
        .collect::<HashMap<_, _>>();

    let mut idom_inv = idom
        .iter()
        .map(|(bb_id, _)| (*bb_id, None))
        .collect::<HashMap<_, _>>();
    for (a, bs) in &idom {
        for b in bs {
            debug_assert!(idom_inv[b].is_none());
            *idom_inv.get_mut(b).unwrap() = Some(*a);
        }
    }

    for bb in &func.bbs {
        let a = bb.id;
        for b in bb.term().nexts() {
            let mut x = a;
            while !sdom[&x].contains(&b) {
                df.get_mut(&x).unwrap().insert(b);
                x = idom_inv[&x].unwrap();
            }
        }
    }

    df
}
