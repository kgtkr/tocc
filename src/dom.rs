use std::{
    collections::{HashMap, HashSet},
    println,
};

type Graph = HashMap<usize, HashSet<usize>>;

fn dom(entry: usize, cfg: &Graph) -> HashMap<usize, HashSet<usize>> {
    // TODO: reg_allocと重複
    let mut preds = cfg
        .iter()
        .map(|(a, _)| (a, HashSet::new()))
        .collect::<HashMap<_, _>>();
    for (a, edges) in cfg {
        for b in edges {
            preds.get_mut(&b).unwrap().insert(a);
        }
    }

    let mut doms = cfg
        .iter()
        .map(|(&a, _)| {
            (a, {
                if a == entry {
                    HashSet::from([a])
                } else {
                    cfg.iter().map(|(a, _)| a).copied().collect()
                }
            })
        })
        .collect::<HashMap<_, _>>();
    let mut changed = true;

    while changed {
        changed = false;
        for (a, edges) in cfg.iter().filter(|(&a, _)| a != entry) {
            let mut new_dom = preds[a]
                .iter()
                .map(|pred| doms[pred].clone())
                .fold::<Option<HashSet<usize>>, _>(None, |acc, dom| {
                    if let Some(acc) = acc {
                        Some(acc.intersection(&dom).copied().collect())
                    } else {
                        Some(dom)
                    }
                })
                .unwrap();
            new_dom.insert(*a);
            if new_dom.len() != doms[a].len() {
                doms.insert(*a, new_dom);
                changed = true;
            }
        }
    }

    doms
}

fn sdom(entry: usize, cfg: &Graph) -> HashMap<usize, HashSet<usize>> {
    let doms = dom(entry, cfg);
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

fn idom(sdoms: HashMap<usize, HashSet<usize>>) -> HashMap<usize, Option<usize>> {
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
                let mut iter = sdom
                    .iter()
                    .filter(|b| sdom_preds[b].iter().all(|c| !sdom.contains(c)))
                    .copied();
                let ret = iter.next();
                debug_assert!(iter.next().is_none());
                ret
            })
        })
        .collect::<HashMap<_, _>>();

    idoms
}

fn dom_front(entry: usize, cfg: &Graph) -> HashMap<usize, HashSet<usize>> {
    let sdom = sdom(entry, cfg);
    let idom = idom(sdom.clone());

    let mut df = cfg
        .iter()
        .map(|(a, _)| (*a, HashSet::new()))
        .collect::<HashMap<_, _>>();

    for (&a, edges) in cfg {
        for &b in edges {
            let mut x = a;
            while !sdom[&x].contains(&b) {
                df.get_mut(&x).unwrap().insert(b);
                if let Some(new_x) = idom[&x] {
                    x = new_x;
                } else {
                    break;
                }
            }
        }
    }

    df
}

#[test]
fn test_dom() {
    // https://qiita.com/uint256_t/items/7d4556cb8f5997b9e95c
    let cfg = HashMap::from([
        (1, HashSet::from([2, 5])),
        (2, HashSet::from([3, 4])),
        (3, HashSet::from([6])),
        (4, HashSet::from([6])),
        (5, HashSet::from([6])),
        (6, HashSet::from([])),
    ]);
    let dom_front = dom_front(1, &cfg);
    assert_eq!(
        dom_front,
        HashMap::from([
            (1, HashSet::from([])),
            (2, HashSet::from([6])),
            (3, HashSet::from([6])),
            (4, HashSet::from([6])),
            (5, HashSet::from([])),
            (6, HashSet::from([])),
        ])
    );
}
