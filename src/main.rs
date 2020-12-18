//
// Copyright 2020-Present (c) Raja Lehtihet & Wael El Oraiby
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// 1. Redistributions of source code must retain the above copyright notice,
// this list of conditions and the following disclaimer.
//
// 2. Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
//
// 3. Neither the name of the copyright holder nor the names of its contributors
// may be used to endorse or promote products derived from this software without
// specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//
mod s_exp;
use s_exp::*;

mod vm;
use vm::*;

#[derive(Copy, Clone, Debug)]
pub enum Blah {
    Blah(u32),
    Blooh,
}

fn main() {
    let mut vm = VM::new(512);
    let v = vm.run("123");

    let v2 = vm.run("(+ 1234 8765)");
    let vv2 = &vm[v2];
    println!("test: {}", (vv2.to_string(&vm)));

    let v = vm.run("(let v (lambda (a b) (+ a b)) (v 123 543))");
    assert!(vm[v].to_string(&vm) == "666");

    for i in 0..7 {
        println!("{} --> ", i);
    }

    println!("{}", 12345);
    println!("Hello!!!");

    let mut command = String::new();
    while command != "exit\n" {
        let mut buff = String::new();
        let n = std::io::stdin().read_line(&mut buff).unwrap();
        command = String::from(&buff[0..n]);
        println!("{}", command);
    }
}
