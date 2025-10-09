use num_bigint::BigUint;
use pyo3::{exceptions::PyRuntimeError, prelude::*};
use std::sync::Arc;

use crate::convert::Mappable;

use super::Signal;

pub fn slice(canidate: &Signal, starting: u32, ending: u32) -> PyResult<Signal> {
    let width = 1 + ending.checked_sub(starting).ok_or(PyRuntimeError::new_err(
        "Slicing failed because starting idx was less than ending idx",
    ))?;

    let mut builder = wellen::BitVectorBuilder::from_signal(canidate.signal.as_ref(), width)
        .ok_or(PyRuntimeError::new_err(
            "Trying to slice a signal that is not a bitvector",
        ))?;
    for (idx, val) in canidate.signal.as_ref().iter_changes() {
        let val: BigUint = BigUint::try_from_signal(val).ok_or_else(|| {
            PyRuntimeError::new_err(format!(
                "Could not convert signal value to BigUint, with value {:?}",
                val.to_bit_string().unwrap()
            ))
        })?;

        // Shift right to remove lower bits, then mask off higher bits
        let val = (val >> starting) & ((BigUint::from(1u32) << width) - BigUint::from(1u32));

        let bytes = val.to_bytes_be();
        builder.add_change(idx, wellen::SignalValue::Binary(bytes.as_slice(), width));
    }

    let new_signal = builder.finish(canidate.signal.signal_ref());

    Ok(Signal {
        signal: Arc::new(new_signal),
        all_times: canidate.all_times.clone(),
    })
}
