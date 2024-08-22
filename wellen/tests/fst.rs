// Copyright 2024 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@berkeley.edu>
//
// test fst specific meta data

use std::collections::HashSet;
use wellen::simple::*;
use wellen::*;

fn load_gtkwave_des() -> (Waveform, ScopeRef) {
    let filename = "inputs/gtkwave-analyzer/des.fst";
    let waves = read(filename).expect("failed to parse");
    let h = waves.hierarchy();
    let top = h.first_scope().unwrap();
    assert_eq!("top", top.name(h));
    assert_eq!(Some(("des.v", 18)), top.source_loc(h));
    assert_eq!(Some(("des.v", 18)), top.instantiation_source_loc(h));
    let des_id = top.scopes(h).next().unwrap();
    let des = h.get(des_id);
    assert_eq!("des", des.name(h));
    (waves, des_id)
}

/// Loads a FST file that was generated by Icarus Verilog which annotates
/// scopes with the source location for both, their instance and their declaration.
#[test]
fn test_source_locators() {
    let (waves, des_id) = load_gtkwave_des();
    let h = waves.hierarchy();
    let des = h.get(des_id);

    let mut round_instance_lines = HashSet::new();
    for scope in des.scopes(h).map(|i| h.get(i)) {
        let name = scope.name(h);
        let (loc_path, loc_line) = scope.source_loc(h).expect("should have source loc");
        let (inst_path, inst_line) = scope
            .instantiation_source_loc(h)
            .expect("should have source loc");

        // everything is from a single `des.v` file
        assert_eq!(loc_path, "des.v");
        assert_eq!(inst_path, "des.v");

        // all round instances are from the same module
        if name.starts_with("round") {
            assert_eq!(loc_line, 991);
            assert!(!round_instance_lines.contains(&inst_line));
            round_instance_lines.insert(inst_line);
        }
    }
}

/// Loads a FST file that was generated by Icarus Verilog which _sometimes_ annotates
/// scopes with the name of the function or module that was instantiated.
#[test]
fn test_component_names() {
    let (waves, des_id) = load_gtkwave_des();
    let h = waves.hierarchy();
    let des = h.get(des_id);
    for scope in des.scopes(h).map(|i| h.get(i)) {
        let name = scope.name(h);

        // all rounds are instances of the same round_func
        if name.starts_with("round") {
            assert_eq!(scope.component(h).unwrap(), "roundfunc");
        }
    }
    let key_shed_id = des
        .scopes(h)
        .find(|s| h.get(*s).name(h) == "keysched")
        .unwrap();
    for scope in h.get(key_shed_id).scopes(h).map(|i| h.get(i)) {
        let name = scope.name(h);
        let comp = scope.component(h);
        if name == "pc1" {
            assert!(comp.is_none(), "{:?}", comp);
        } else if name.starts_with("pc2") {
            assert_eq!(comp.unwrap(), "pc2", "{:?}", comp);
        } else {
            let comp = comp.unwrap();
            assert!(comp == "rol1" || comp == "rol2", "{:?}", comp);
        }
    }
}

fn load_verilator_many_sv_datatypes() -> (Waveform, ScopeRef) {
    let filename = "inputs/verilator/many_sv_datatypes.fst";
    let waves = read(filename).expect("failed to parse");
    let h = waves.hierarchy();
    let top = h.first_scope().unwrap();
    assert_eq!("TOP", top.name(h));
    let wrapper = h.get(top.scopes(h).next().unwrap());
    assert_eq!("SVDataTypeWrapper", wrapper.name(h));
    let bb_id = wrapper.scopes(h).next().unwrap();
    let bb = h.get(bb_id);
    assert_eq!("bb", bb.name(h));
    (waves, bb_id)
}

/// Loads a FST file that was generated by Verilator which annotates enum signals.
#[test]
fn test_enum_signals() {
    let (waves, bb_id) = load_verilator_many_sv_datatypes();
    let h = waves.hierarchy();
    let bb = h.get(bb_id);
    let abc_r = bb
        .vars(h)
        .map(|i| h.get(i))
        .find(|v| v.name(h) == "abc_r")
        .expect("failed to find abc_r");
    let mut enum_type = abc_r.enum_type(h).expect("abc_r should have an enum type!");
    assert_eq!(enum_type.0, "SVDataTypeBlackBox.abc");
    enum_type.1.sort();
    assert_eq!(
        enum_type.1,
        [("00", "A"), ("01", "B"), ("10", "C"), ("11", "D")]
    );
}

/// Loads a FST file that was generated by Verilator which contains variable directions.
#[test]
fn test_var_directions() {
    let (waves, bb_id) = load_verilator_many_sv_datatypes();
    let h = waves.hierarchy();
    let bb = h.get(bb_id);

    for var_ref in bb.vars(h) {
        let var = h.get(var_ref);
        let dt = (var.direction(), var.var_type());
        match var.name(h) {
            "abc_r" => assert_eq!(dt, (VarDirection::Implicit, VarType::Logic)),
            "clock" => assert_eq!(dt, (VarDirection::Input, VarType::Wire)),
            "int_r" => assert_eq!(dt, (VarDirection::Implicit, VarType::Integer)),
            "out" => assert_eq!(dt, (VarDirection::Output, VarType::Wire)),
            "real_r" => assert_eq!(dt, (VarDirection::Implicit, VarType::Real)),
            "time_r" => assert_eq!(dt, (VarDirection::Implicit, VarType::Bit)),
            other => todo!("{other} {dt:?}"),
        }
    }
}

// See: https://gitlab.com/surfer-project/surfer/-/issues/201
// This file contains two separate declarations of the `top` scope which need to be merged
// automatically by `wellen`.
#[test]
fn test_scope_merging() {
    let waves = read("inputs/verilator/surfer_issue_201.fst").unwrap();
    let h = waves.hierarchy();
    assert_eq!(h.scopes().count(), 1);
    assert_eq!(h.vars().count(), 0);
    let toplevel = h.first_scope().unwrap();
    assert_eq!(toplevel.name(h), "TOP");
    let top_scopes: Vec<_> = toplevel.scopes(h).map(|i| h.get(i).name(h)).collect();
    assert_eq!(top_scopes, ["top", "svfloat::ffunc"]);
    let top_vars: Vec<_> = toplevel.vars(h).map(|i| h.get(i).name(h)).collect();
    assert_eq!(top_vars, ["clk"]);

    let top_top = h.get(toplevel.scopes(h).next().unwrap());
    assert_eq!(top_top.full_name(h), "TOP.top");
    let top_top_scopes: Vec<_> = top_top.scopes(h).map(|i| h.get(i).name(h)).collect();
    assert_eq!(top_top_scopes, ["vga_gen", "pix_port", "vga_port"]);

    let vga_gen = h.get(top_top.scopes(h).next().unwrap());
    assert_eq!(vga_gen.full_name(h), "TOP.top.vga_gen");
    let vga_gen_scopes: Vec<_> = vga_gen.scopes(h).map(|i| h.get(i).name(h)).collect();
    assert_eq!(
        vga_gen_scopes,
        [
            "pix_fsm_x",
            "pix_fsm_y",
            "sync_fsm_x",
            "sync_fsm_y",
            "pix",
            "vga"
        ]
    )
}

/// This file was provided by Augusto Fraga Giachero in the following issue:
/// https://github.com/ekiwi/wellen/issues/9
#[test]
fn test_nvc_xwb_fofb_shaper_filt_tb() {
    let _waves = read("inputs/nvc/xwb_fofb_shaper_filt_tb.fst").unwrap();
}

fn load_all_signals(waves: &mut Waveform) {
    let all_signals = waves
        .hierarchy()
        .get_unique_signals_vars()
        .iter()
        .flatten()
        .map(|v| v.signal_ref())
        .collect::<Vec<_>>();
    waves.load_signals(&all_signals);
}

/// This file was provided by Augusto Fraga Giachero in the following issue:
/// https://github.com/ekiwi/wellen/issues/16
#[test]
fn test_nvc_vhdl_test_bool_issue_16() {
    // turns out that NVC needs a little hack to deal with the way it encodes booleans
    let mut waves = read("inputs/nvc/vhdl_test_bool_issue_16.fst").unwrap();
    load_all_signals(&mut waves);

    // check signal values
    let h = waves.hierarchy();
    let toplevel = h.first_scope().unwrap();
    let var = h.get(toplevel.vars(h).next().unwrap());
    assert_eq!(var.full_name(h), "test_bool.bool");
    let time_and_values = waves
        .get_signal(var.signal_ref())
        .unwrap()
        .iter_changes()
        .map(|(time, value)| format!("{time} {}", value.to_string()))
        .collect::<Vec<_>>();
    assert_eq!(
        time_and_values,
        [
            // the boolean is encoded as string literals by nvc
            "0 false", "1 true", "2 false"
        ]
    );
}

/// This file was provided by Tiago Gomes in the following issue:
/// https://github.com/ekiwi/wellen/issues/21
#[test]
fn test_nvc_overlay_tb_issue_21() {
    // This used to crash because one of the variables used an index that does not fit into 32 bits.
    let mut waves = read("inputs/nvc/overlay_tb_issue_21.fst").unwrap();
    load_all_signals(&mut waves);
}
