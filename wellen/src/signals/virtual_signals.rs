use std::{mem::size_of, ops::Div};

use super::{Real, Signal, SignalChangeData, SignalValue, TimeTableIdx};

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
                SignalValue::Real(real) => SignalValue::Real(real.clone()),
            },
        }
    }
}

enum OwnedSignalValue {
    Binary(Box<[u8]>, u32),
    FourValue(Box<[u8]>, u32),
    NineValue(Box<[u8]>, u32),
    String(String),
    Real(Real),
}

impl OwnedSignalValue {
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
            OwnedSignalValue::Real(real) => SignalValue::Real(real.clone()),
        }
    }
}

pub trait SignalView {
    fn get_signal(&self, time_offset: TimeTableIdx) -> Option<SignalViewValue>;
}

struct Slice<V> {
    var: V,
    slice_ub: u32,
    slice_lb: u32,
}

impl<V> SignalView for Slice<V>
where
    V: SignalView,
{
    fn get_signal(&self, time_offset: TimeTableIdx) -> Option<SignalViewValue> {
        fn slice_value(sig: SignalValue<'_>, lb: u32, ub: u32) -> OwnedSignalValue {
            match sig {
                SignalValue::Binary(bits, _) => {
                    let lb_idx = lb.div(8);
                    let ub_idx = ub.div(8);
                    let width = ub - lb;
                    let new_data = &bits[lb_idx as usize..=ub_idx as usize];
                    let data = new_data.into();
                    OwnedSignalValue::Binary(data, width)
                }

                SignalValue::FourValue(..)
                | SignalValue::NineValue(..)
                | SignalValue::Real(_)
                | SignalValue::String(_) => {
                    unimplemented!()
                }
            }
        }

        if let Some(sig) = self.var.get_signal(time_offset) {
            let sig = sig.signal_value();
            Some(SignalViewValue::Owned(slice_value(
                sig,
                self.slice_lb,
                self.slice_ub,
            )))
        } else {
            None
        }
    }
}

impl SignalView for Signal {
    fn get_signal(&self, time_offset: TimeTableIdx) -> Option<SignalViewValue> {
        self.get_offset(time_offset)
            .map(|val| self.get_value_at(&val, 1))
            .map(|val| SignalViewValue::Borrowed(val))
    }
}

pub struct VirtualSignal {
    name: String,
    data: SignalChangeData,
    time_indices: Vec<TimeTableIdx>,
}
