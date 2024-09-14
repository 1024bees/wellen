use std::collections::HashMap;

use itertools::Itertools;

use crate::SignalRef;

use super::{BitVectorBuilder, Real, Signal, SignalValue, TimeTableIdx};

pub enum SignalViewValue<'a> {
    Owned(OwnedSignalValue),
    Borrowed(SignalValue<'a>),
}

impl<'a> SignalViewValue<'a> {
    fn signal_value(&'a self) -> SignalValue<'a> {
        match self {
            Self::Owned(owned) => owned.as_signal_value(),
            Self::Borrowed(borrowed) => match borrowed {
                SignalValue::Binary(binval, size) => SignalValue::Binary(binval, *size),
                SignalValue::FourValue(fourval, size) => SignalValue::FourValue(fourval, *size),
                SignalValue::NineValue(nineval, size) => SignalValue::NineValue(nineval, *size),
                SignalValue::String(strval) => SignalValue::String(strval),
                SignalValue::Real(real) => SignalValue::Real(*real),
            },
        }
    }
}

pub enum OwnedSignalValue {
    Binary(Box<[u8]>, u32),
    FourValue(Box<[u8]>, u32),
    NineValue(Box<[u8]>, u32),
    String(String),
    Real(Real),
}

impl OwnedSignalValue {
    fn set_signal_size(self, bit_width: u32) -> Self {
        match self {
            OwnedSignalValue::Binary(binval, _size) => OwnedSignalValue::Binary(binval, bit_width),
            OwnedSignalValue::FourValue(fourval, _siz) => {
                OwnedSignalValue::FourValue(fourval, bit_width)
            }
            OwnedSignalValue::NineValue(nineval, _size) => {
                OwnedSignalValue::NineValue(nineval, bit_width)
            }
            _ => self,
        }
    }
    fn as_signal_value(&self) -> SignalValue<'_> {
        match self {
            OwnedSignalValue::Binary(binval, size) => SignalValue::Binary(binval.as_ref(), *size),
            OwnedSignalValue::FourValue(fourval, size) => {
                SignalValue::FourValue(fourval.as_ref(), *size)
            }
            OwnedSignalValue::NineValue(nineval, size) => {
                SignalValue::NineValue(nineval.as_ref(), *size)
            }
            OwnedSignalValue::String(strval) => SignalValue::String(strval.as_str()),
            OwnedSignalValue::Real(real) => SignalValue::Real(*real),
        }
    }
}

pub trait SignalView {
    fn get_signal(&self, time_offset: TimeTableIdx) -> Option<SignalViewValue>;
}

impl SignalView for Signal {
    fn get_signal(&self, time_offset: TimeTableIdx) -> Option<SignalViewValue> {
        self.get_offset(time_offset)
            .map(|val| self.get_value_at(&val, 1))
            .map(SignalViewValue::Borrowed)
    }
}

pub struct VirtualSignalBuilder {
    live_signals: HashMap<String, Signal>,
    sig_generator: Box<dyn FnMut(&HashMap<String, Signal>, TimeTableIdx) -> SignalValue>,
    builder: BitVectorBuilder,
}

impl VirtualSignalBuilder {
    fn to_signal(self, id: SignalRef) -> Signal {
        let mut builder = self.builder;
        let mut gen = self.sig_generator;
        let uniq_idx: Vec<u32> = self
            .live_signals
            .values()
            .flat_map(|val| val.time_indices())
            .unique()
            .cloned()
            .collect();
        for idx in uniq_idx {
            let val = gen(&self.live_signals, idx);
            builder.add_change(idx, val)
        }
        builder.finish(id)
    }
}
