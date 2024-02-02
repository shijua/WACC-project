fn is_valid_single_ascii(chr: u8) -> bool {
    (chr >= 0x20 && chr < 0x80) && (chr != b'\'') && (chr != b'\"') && (chr != b'\\')
}
