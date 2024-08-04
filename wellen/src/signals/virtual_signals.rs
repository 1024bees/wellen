use std::{
    collections::HashMap,
    mem::size_of,
    ops::{BitOr, Div},
};

use itertools::Itertools;

use super::{
    arithmetic::{binary_op, Biter},
    BitVectorBuilder, Real, Signal, SignalChangeData, SignalValue, TimeTableIdx,
};

pub enum SignalViewValue<'a> {
    Owned(OwnedSignalValue),
    Borrowed(SignalValue<'a>),
}

pub trait Mappable: Sized {
    fn from_signal(signal_value: SignalValue<'_>) -> Option<Self>;
    fn into_signal(&self) -> SignalValue;
    //fn into_sized_signal(&self, num_bits: u32) -> OwnedSignalValue {
    //    self.into_signal().set_signal_size(num_bits)
    //}
    fn bit_width(&self) -> u32 {
        std::mem::size_of::<Self>() as u32
    }
}

impl Mappable for u16 {
    fn from_signal(signal_value: SignalValue<'_>) -> Option<Self> {
        match signal_value {
            SignalValue::Binary(val, bits) => {
                if bits >= std::mem::size_of::<Self>() as u32 {
                    let val = val.try_into().ok().map(|val| u16::from_be_bytes(val));
                    val
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn into_signal(&self) -> SignalValue {
        const ARRAY_SIZE: usize = std::mem::size_of::<u16>();
        let value: &[u8; ARRAY_SIZE] = bytemuck::cast_ref(self);
        SignalValue::Binary(value.as_slice(), self.bit_width())
    }
}

impl Mappable for f32 {
    fn from_signal(signal_value: SignalValue<'_>) -> Option<Self> {
        match signal_value {
            SignalValue::Real(val) => Some(val as f32),
            SignalValue::Binary(val, bits) => {
                if bits == 32 {
                    let val = val.try_into().ok().map(|val| f32::from_be_bytes(val));
                    val
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn into_signal(&self) -> SignalValue {
        const ARRAY_SIZE: usize = std::mem::size_of::<f32>();
        let value: &[u8; ARRAY_SIZE] = bytemuck::cast_ref(self);
        SignalValue::Binary(value.as_slice(), self.bit_width())
    }
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

pub enum OwnedSignalValue {
    Binary(Box<[u8]>, u32),
    FourValue(Box<[u8]>, u32),
    NineValue(Box<[u8]>, u32),
    String(String),
    Real(Real),
}

impl<'a> BitOr for SignalViewValue<'a> {
    type Output = OwnedSignalValue;
    fn bitor(self, rhs: Self) -> Self::Output {
        let rhs = self.signal_value();
        let lhs = self.signal_value();
        match (lhs, rhs) {
            (
                SignalValue::Binary(_, _)
                | SignalValue::FourValue(_, _)
                | SignalValue::NineValue(_, _),
                SignalValue::Binary(_, _)
                | SignalValue::FourValue(_, _)
                | SignalValue::NineValue(_, _),
            ) => {
                let lhs_iter = Biter::from_signal_value(&lhs).unwrap();
                let rhs_iter = Biter::from_signal_value(&rhs).unwrap();
                binary_op(lhs_iter, rhs_iter, |l, r| l | r)
            }
            (_, _) => {
                todo!()
            }
        }
    }
}

impl OwnedSignalValue {
    fn set_signal_size(self, bit_width: u32) -> Self {
        match self {
            OwnedSignalValue::Binary(binval, size) => OwnedSignalValue::Binary(binval, bit_width),
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

pub struct VirtualSignalBuilder {
    live_signals: HashMap<String, Signal>,
    sig_generator: Box<dyn FnMut(&HashMap<String, Signal>, TimeTableIdx) -> SignalValue>,
    builder: BitVectorBuilder,
}

impl VirtualSignalBuilder {
    fn to_signal(mut self) {
        let mut builder = self.builder;
        let mut gen = self.sig_generator;
        let uniq_idx: Vec<u32> = self
            .live_signals
            .values()
            .map(|val| val.time_indices())
            .flatten()
            .unique()
            .cloned()
            .collect();
        for idx in uniq_idx {
            let val = gen(&self.live_signals, idx);
            builder.add_change(idx, val)
        }
    }
}

pub struct VirtualSignal {
    name: String,
    data: SignalChangeData,
    time_indices: Vec<TimeTableIdx>,
}

#[cfg(test)]
mod tests {
    use super::Mappable;
    use crate::SignalValue;

    #[test]
    fn test_long_2_state_to_string() {
        let data = [0b0, 0b110001, 0b10110011];

        let out = SignalValue::Binary(data.as_slice(), 12);
        let val = u16::from_signal(out).unwrap();
    }
}
