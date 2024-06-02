use crate::SignalValue;

use super::virtual_signals::OwnedSignalValue;

pub(crate) struct Biter<'a> {
    bit_idx: u32,
    byte_idx: usize,
    sub_byte_bit_idx: u32,
    bits_per_byte: u32,
    mask: u8,
    pub(crate) sig_length: u32,
    value: &'a [u8],
}

impl<'a> Biter<'a> {
    pub(crate) fn from_signal_value<'b: 'a>(sig_value: &'b SignalValue<'a>) -> Option<Biter<'a>> {
        let states = sig_value.states()?;
        let bits = sig_value.bits()?;
        let bits_per_byte = states.bits_in_a_byte() as u32;

        let mask = states.mask();

        let byte0_bits = bits - ((bits / bits_per_byte) * bits_per_byte);
        let byte0_is_special = byte0_bits > 0;
        let sub_byte_bit_idx = if byte0_is_special {
            byte0_bits
        } else {
            bits_per_byte
        };
        Some(Self {
            bit_idx: 0,
            byte_idx: 0,
            sub_byte_bit_idx,
            mask,
            sig_length: bits,
            bits_per_byte,
            value: sig_value.data()?,
        })
    }

    fn new_sig(bits: u32, bits_per_byte: u32) -> Self {
        let byte0_bits = bits - ((bits / bits_per_byte) * bits_per_byte);
        let byte0_is_special = byte0_bits > 0;
        let sub_byte_bit_idx = if byte0_is_special {
            byte0_bits
        } else {
            bits_per_byte
        };

        Self {
            bit_idx: 0,
            byte_idx: 0,
            sub_byte_bit_idx,
            mask: 1,
            sig_length: bits,
            bits_per_byte,
            value: &[],
        }
    }

    pub(crate) fn into_padded_iter(self, required_len: u32) -> impl Iterator<Item = BitValue> + 'a {
        let required_padding = required_len.saturating_sub(self.sig_length);
        ZeroBiter::new(required_padding).into_iter().chain(self)
    }
    fn advance(&mut self) {
        self.bit_idx += 1;
        if self.sub_byte_bit_idx == 0 {
            self.sub_byte_bit_idx = self.bits_per_byte - 1;
            self.byte_idx += 1;
        } else {
            self.sub_byte_bit_idx -= 1;
        }
        self.bit_idx += 1;
    }
}

struct BitSetter {
    iter_logic: Biter<'static>,
}
impl BitSetter {
    fn new_sig(bits: u32, bits_per_byte: u32) -> Self {
        let byte0_bits = bits - ((bits / bits_per_byte) * bits_per_byte);
        let byte0_is_special = byte0_bits > 0;
        let sub_byte_bit_idx = if byte0_is_special {
            byte0_bits
        } else {
            bits_per_byte
        };

        Self {
            iter_logic: Biter {
                bit_idx: 0,
                byte_idx: 0,
                sub_byte_bit_idx,
                mask: 1,
                sig_length: bits,
                bits_per_byte,
                value: &[],
            },
        }
    }
    fn set_and_adv(&mut self, value: BitValue, payload: &mut [u8]) -> Option<()> {
        if self.iter_logic.sig_length > self.iter_logic.bit_idx {
            payload[self.iter_logic.byte_idx] |=
                (value as u8) << (self.iter_logic.sub_byte_bit_idx * self.iter_logic.bits_per_byte);
            self.iter_logic.advance();
            Some(())
        } else {
            None
        }
    }
}

pub struct BinaryBiter<A, B, C>
where
    A: Iterator<Item = BitValue>,
    B: Iterator<Item = BitValue>,
    C: FnMut(BitValue, BitValue) -> BitValue,
{
    first: A,
    second: B,
    len: u32,
    bits_per_byte: u32,
    bin_operation: C,
}

pub(crate) fn binary_op(
    first: Biter<'_>,
    second: Biter<'_>,
    binary_op: impl Fn(BitValue, BitValue) -> BitValue,
) -> OwnedSignalValue {
    let output_sig_len = first.sig_length.max(second.sig_length);
    let bits_per_byte = first.bits_per_byte.min(second.bits_per_byte);
    let lhs_iter = first.into_padded_iter(output_sig_len);
    let rhs_iter = second.into_padded_iter(output_sig_len);
    let iter = lhs_iter.zip(rhs_iter).map(|(l, r)| binary_op(l, r));
    let payload_size = output_sig_len / bits_per_byte;
    let mut payload = vec![0u8; payload_size as usize].into_boxed_slice();
    let mut setter = BitSetter::new_sig(output_sig_len, bits_per_byte);
    for val in iter {
        setter.set_and_adv(val, &mut payload);
    }
    match bits_per_byte {
        8 => OwnedSignalValue::Binary(payload, output_sig_len),
        4 => OwnedSignalValue::FourValue(payload, output_sig_len),
        2 => OwnedSignalValue::NineValue(payload, output_sig_len),
        _ => unreachable!("Invalid bits per byte"),
    }
}

impl Iterator for Biter<'_> {
    type Item = BitValue;
    fn next(&mut self) -> Option<Self::Item> {
        if self.sig_length > self.bit_idx {
            let byte = self.value[self.byte_idx];
            let rv = (byte >> (self.sub_byte_bit_idx * self.bits_per_byte)) & self.mask;
            self.advance();
            Some(BitValue::from_u8(rv))
        } else {
            None
        }
    }
}

struct ZeroBiter {
    req_zeroes: u32,
    sent_zeroes: u32,
}

impl ZeroBiter {
    fn new(req_zeroes: u32) -> Self {
        Self {
            req_zeroes,
            sent_zeroes: 0,
        }
    }
}

impl Iterator for ZeroBiter {
    type Item = BitValue;
    fn next(&mut self) -> Option<Self::Item> {
        if self.req_zeroes > self.sent_zeroes {
            self.sent_zeroes += 1;
            Some(BitValue::from_u8(0))
        } else {
            None
        }
    }
}

impl std::ops::BitOr for BitValue {
    type Output = BitValue;
    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Zero | Self::One, Self::Zero | Self::One) => {
                BitValue::from_u8(self as u8 | rhs as u8)
            }
            _ => {
                let lhs = self as u8;
                let rhs = rhs as u8;
                BitValue::from_u8(lhs.max(rhs))
            }
        }
    }
}

impl std::ops::BitAnd for BitValue {
    type Output = BitValue;
    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Zero | Self::One, Self::Zero | Self::One) => {
                BitValue::from_u8(self as u8 & rhs as u8)
            }
            _ => {
                let lhs = self as u8;
                let rhs = rhs as u8;
                BitValue::from_u8(lhs.max(rhs))
            }
        }
    }
}

impl std::ops::BitXor for BitValue {
    type Output = BitValue;
    fn bitxor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Zero | Self::One, Self::Zero | Self::One) => {
                BitValue::from_u8(self as u8 ^ rhs as u8)
            }
            _ => {
                let lhs = self as u8;
                let rhs = rhs as u8;
                BitValue::from_u8(lhs.max(rhs))
            }
        }
    }
}

#[repr(u8)]
#[derive(Clone, Copy)]
pub(crate) enum BitValue {
    Zero = 0,
    One = 1,
    X = 2,
    Z = 3,
    H = 4,
    U = 5,
    W = 6,
    L = 7,
    Dash = 8,
}
impl BitValue {
    fn from_u8(val: u8) -> Self {
        match val {
            0 => BitValue::Zero,
            1 => BitValue::One,
            2 => BitValue::X,
            3 => BitValue::Z,
            4 => BitValue::H,
            5 => BitValue::U,
            6 => BitValue::W,
            7 => BitValue::L,
            8 => BitValue::Dash,
            _ => unreachable!("Invalid value for BitValue: {}", val),
        }
    }
}
