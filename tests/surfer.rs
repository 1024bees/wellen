// Copyright 2024 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@berkeley.edu>

use wellen::*;

#[test]
fn surfer_verilator_empty_scope_flatten_scope_with_empty_name() {
    let filename = "inputs/surfer/verilator_empty_scope.vcd";
    let opts = vcd::LoadOptions {
        multi_thread: true,
        remove_scopes_with_empty_name: true,
    };
    let waves = vcd::read_with_options(filename, opts).expect("failed to parse");
    let h = waves.hierarchy();
    let top = h.first_scope().expect("failed to find first scope");
    assert_eq!(top.name(h), "top_test");
}

#[test]
fn surfer_verilator_empty_scope_do_not_flatten_by_default() {
    let filename = "inputs/surfer/verilator_empty_scope.vcd";
    let waves = vcd::read(filename).expect("failed to parse");
    let h = waves.hierarchy();
    let top = h.first_scope().expect("failed to find first scope");
    assert_eq!(top.name(h), "");
}
