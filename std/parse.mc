@public
type ParseError = {}

fn parse_decimal_digit_or_invalid(b: u8) -> u64 {
    let d0: u8 = 48;
    let d1: u8 = 49;
    let d2: u8 = 50;
    let d3: u8 = 51;
    let d4: u8 = 52;
    let d5: u8 = 53;
    let d6: u8 = 54;
    let d7: u8 = 55;
    let d8: u8 = 56;
    let d9: u8 = 57;

    if b == d0 {
        return 0;
    } else if b == d1 {
        return 1;
    } else if b == d2 {
        return 2;
    } else if b == d3 {
        return 3;
    } else if b == d4 {
        return 4;
    } else if b == d5 {
        return 5;
    } else if b == d6 {
        return 6;
    } else if b == d7 {
        return 7;
    } else if b == d8 {
        return 8;
    } else if b == d9 {
        return 9;
    } else {
        return 10;
    }
}

@public
fn parse_u64(text: string) -> u64 | ParseError {
    if text.len == 0 {
        return ParseError {};
    };

    let max_div10: u64 = 1844674407370955161;
    let max_mod10: u64 = 5;

    var value: u64 = 0;
    var i: u64 = 0;
    while i < text.len {
        let b = text[i];
        let digit = parse_decimal_digit_or_invalid(b);
        if digit > 9 {
            return ParseError {};
        };
        if value > max_div10 {
            return ParseError {};
        };
        if value == max_div10 && digit > max_mod10 {
            return ParseError {};
        };

        value = value * 10 + digit;
        i += 1;
    }

    value
}
