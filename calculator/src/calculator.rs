use std::io::{self, BufReader};

use crate::{cmd_stream::CmdStream, input_stream::InputStream, out_stream::OutputStream};

struct Calculator {
    operation_mode: i8,

    // memory
    // TOOD: Add `registers: RegisterSet,`
    // TODO: Add datastack

    // streams
    out_stream: OutputStream<io::Stdout>,
    in_stream: InputStream<BufReader<io::Stdin>>,
    cmd_stream: CmdStream,
}
