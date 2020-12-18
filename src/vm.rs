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
use crate::s_exp::*;

#[derive(Copy, Clone)]
pub struct ExpRef(pub u32);

pub trait ExpAllocator {
    fn add(&mut self, exp: Exp) -> ExpRef;
    fn get(&self, eref: ExpRef) -> &Exp;
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct CellRef(pub u32);

pub trait CellAllocator {
    fn add(&mut self, cell: Cell) -> CellRef;
    fn get(&self, cref: CellRef) -> &Cell;
    fn gc(&mut self);
    fn allocated_cells(&self) -> usize;
}

pub enum Cell {
    Empty,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Symbol(String),
    NativeLambda {
        is_macro: bool,
        args: CellRef,
        func: extern "C" fn(&mut VM) -> CellRef,
    }, // passed arguments are evaluated!
    Lambda {
        is_macro: bool,
        args: CellRef,
        body: CellRef,
    },
    Nil,
    Cons {
        head: CellRef,
        tail: CellRef,
    },
    Forward(CellRef), // Just for GC
}

impl Cell {
    pub fn to_string(&self, vm: &VM) -> String {
        match self {
            Self::Bool(b) => if *b { String::from("#t") } else { String::from("#f") },
            Self::Int(i) => format!("{}", i),
            Self::Float(f)  => format!("{}", f),
            Self::String(s) => format!("{}", s),
            Self::Symbol(s) => s.clone(),
            Self::Nil => String::from("nil"),
            Self::Cons { head: h, tail: t} => {
                let tail = vm.list_to_array(*t);
                let mut s = String::new();
                s.push('(');
                s += &(vm[*h].to_string(vm));
                if tail.len() != 0 { s.push(' ') }

                for i in 0..tail.len() {
                    s += &(vm[tail[i]].to_string(vm));
                    if i != tail.len() - 1 { s.push(' ') }
                }
                s.push(')');
                s
            },
            Self::Empty => String::from("$$EMPTY$$"), //panic!("cannot string empty"),
            Self::NativeLambda { is_macro: m, args: a, func: _ } => {
                let mut s = String::from(if *m { "($$NativeMacro$$ " } else { "($$NativeLambda$$ " });
                let tail = vm.list_to_array(*a);
                s.push('(');
                 for i in 0..tail.len() {
                    s += &(vm[tail[i]].to_string(vm));
                    if i != tail.len() - 1 { s.push(' ') }
                }
                s.push(')');
                s.push(')');
                s
            },
            Self::Lambda { is_macro: m, args: a, body: b } => {
                let mut s = String::from(if *m { "($$Macro$$ " } else { "($$Lambda$$ " });
                let tail = vm.list_to_array(*a);
                s.push('(');
                 for i in 0..tail.len() {
                    s +=  &(vm[tail[i]].to_string(vm));
                    if i != tail.len() - 1 { s.push(' ') }
                }
                s.push(')');

                let body = vm.list_to_array(*b);
                s.push('(');
                 for i in 0..body.len() {
                    s += &(vm[body[i]].to_string(vm));
                    if i != body.len() - 1 { s.push(' ') }
                }
                s.push(')');
                s.push(')');
                s
            }
            Self::Forward(_) => panic!("cannot string it")
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

pub struct Binding {
    pub var: String,
    pub value: CellRef,
}

pub struct Env {
    syms        : Vec<Binding>,
    bind_frames : Vec<usize>,
    temp        : Vec<CellRef>,
    temp_frames : Vec<usize>,
}


impl Env {
    pub fn new() -> Self {
        Self {
            syms        : Vec::new(),
            bind_frames : Vec::new(),
            temp        : Vec::new(),
            temp_frames : Vec::new(),
        }
    }

    pub fn push_binding(&mut self, var: String, value: CellRef) -> usize {
        self.syms.push(Binding {
            var: var,
            value: value,
        });
        self.syms.len() - 1
    }

    pub fn push_bind_frame(&mut self) {
        self.bind_frames.push(self.syms.len())
    }

    pub fn pop_bind_frame(&mut self) {
        let count = self.syms.len() - self.bind_frames[self.bind_frames.len() - 1];
        for _ in 0..count {
            self.syms.pop();
        }
        self.bind_frames.pop();
    }

    pub fn push_temp_frame(&mut self) {
        self.temp_frames.push(self.temp.len())
    }

    pub fn pop_temp_frame(&mut self) {
        assert!(self.temp_frames.len() > 0);
        let count = self.temp.len() - self.temp_frames[self.temp_frames.len() - 1];
        for _ in 0..count {
            self.temp.pop();
        }
        self.temp_frames.pop();
    }

    pub fn push_temp(&mut self, r: CellRef) -> usize {
        let c = self.temp.len();
        self.temp.push(r);
        c
    }

    pub fn value(&self, sym: &str) -> Option<CellRef> {
        let len = self.syms.len();
        for i in 0..len {
            let s = &self.syms[len - i - 1];
            if s.var == sym {
                return Some(s.value);
            }
        }
        None
    }

    pub fn syms_as_mut(&mut self) -> &mut Vec<Binding> {
        &mut self.syms
    }
}

impl core::ops::Index<usize> for Env {
    type Output = Binding;
    #[inline]
    fn index(&self, idx: usize) -> &Self::Output {
        &self.syms[idx]
    }
}

impl core::ops::IndexMut<usize> for Env {
    #[inline]
    fn index_mut(&mut self, idx: usize) -> &mut Self::Output {
        &mut self.syms[idx]
    }
}

////////////////////////////////////////////////////////////////////////////////

struct SemiSpace {
    cells: *mut Cell,
    len: usize, // max element count
    cursor: usize,
}

impl SemiSpace {
    pub fn new(len: usize) -> Self {
        unsafe {
            let cells = std::alloc::alloc(std::alloc::Layout::array::<Cell>(len).unwrap()) as *mut Cell;
            let arr = core::slice::from_raw_parts_mut(cells, len);
            for i in 0..len {
                arr[i] = Cell::Empty
            }
            Self {
                cells: cells,
                len: len,
                cursor: 0,
            }
        }
    }

    #[inline]
    pub fn cursor(&self) -> usize {
        self.cursor
    }

    #[inline]
    pub fn get(&self, idx: usize) -> &Cell {
        unsafe { &core::slice::from_raw_parts(self.cells, self.len)[idx] }
    }

    #[inline]
    pub fn get_mut(&mut self, idx: usize) -> &mut Cell {
        unsafe { &mut core::slice::from_raw_parts_mut(self.cells, self.len)[idx] }
    }

    pub fn reset(&mut self) {
        self.cursor = 0;
        for i in 0..self.len {
            self[i] = Cell::Empty;
        }
    }

    pub fn alloc(&mut self) -> CellRef {
        self.cursor += 1;
        if self.cursor >= self.len {
            panic!("outside boundaries!!!"); // TODO: reallocate
        }
        CellRef((self.cursor as u32) - 1)
    }

    pub fn len(&self) -> usize {
        self.len
    }
}

impl Drop for SemiSpace {
    fn drop(&mut self) {
        unsafe { std::alloc::dealloc(self.cells as *mut u8, std::alloc::Layout::array::<Cell>(self.len).unwrap()) }
    }
}

impl core::ops::Index<CellRef> for SemiSpace {
    type Output = Cell;
    #[inline]
    fn index(&self, idx: CellRef) -> &Self::Output {
        self.get(idx.0 as usize)
    }
}

impl core::ops::IndexMut<CellRef> for SemiSpace {
    #[inline]
    fn index_mut(&mut self, idx: CellRef) -> &mut Self::Output {
        self.get_mut(idx.0 as usize)
    }
}

impl core::ops::Index<usize> for SemiSpace {
    type Output = Cell;
    #[inline]
    fn index(&self, idx: usize) -> &Self::Output {
        self.get(idx)
    }
}

impl core::ops::IndexMut<usize> for SemiSpace {
    #[inline]
    fn index_mut(&mut self, idx: usize) -> &mut Self::Output {
        self.get_mut(idx)
    }
}

////////////////////////////////////////////////////////////////////////////////
/// Implements Cheney's semi-space garbage collector algorithm
////////////////////////////////////////////////////////////////////////////////
pub struct Heap {
    active: SemiSpace,
    to_space: SemiSpace,
}

impl Heap {
    pub fn new(heap_size: usize) -> Self {
        Self {
            active: SemiSpace::new(heap_size),
            to_space: SemiSpace::new(heap_size),
        }
    }

    pub fn size(&self) -> usize {
        self.active.len()
    }

    #[inline]
    fn swap_spaces(&mut self) {
        ::core::mem::swap(&mut self.active, &mut self.to_space);
        self.to_space.reset();
    }

    fn move_object(&mut self, cref: CellRef) -> CellRef {
        match &self.active[cref] {
            &Cell::Forward(r) => r,
            &Cell::Empty => panic!("Invalid move!"),
            _ => {
                let r = self.to_space.alloc();
                let mut t = Cell::Forward(r);
                core::mem::swap(&mut t, &mut self.active[cref]);
                self.to_space[r] = t;
                match self.active[cref] {
                    Cell::Forward(x) if x.0 == r.0 => (),
                    _ => panic!("eexpected forward r")
                }
                r
            }
        }
    }

    // TODO: pinning, especially if we are collecting when we are in the process of allocation
    pub fn collect(&mut self, env: &mut Env) {
        // start by moving the environment
        for t in 0..env.syms.len() {
            env.syms[t].value = self.move_object(env.syms[t].value);
        }

        for s in 0..env.temp.len() {
            env.temp[s]       = self.move_object(env.temp[s]);
        }

        // now continue moving all the rest from active to space, reading refs from toSpace
        let mut t = 0;
        loop {
            if t >= self.to_space.cursor() {
                break;
            }

            match self.to_space[t] {
                Cell::Empty => break,

                Cell::NativeLambda { is_macro: m, args: a, func: f } => {
                    let af = self.move_object(a);
                    self.to_space[t] = Cell::NativeLambda { is_macro: m, args: af, func: f };
                },

                Cell::Lambda { is_macro: m, args: a, body: b } => {
                    let bf = self.move_object(b);
                    let af = self.move_object(a);
                    self.to_space[t] = Cell::Lambda { is_macro: m, args: af, body: bf };
                },

                Cell::Cons { head: h, tail: ta } => {
                    let hf = self.move_object(h);
                    let tf = self.move_object(ta);
                    self.to_space[t] = Cell::Cons { head: hf, tail: tf };
                },

                Cell::Forward(_) => panic!("Impossible: Forward in toSpace"),
                _ => (), // already moved tospace, do nothing
            }

            t += 1;
        }

        // finally swap spaces
        self.swap_spaces();
    }

    pub fn alloc(&mut self) -> Option<CellRef> {
        if self.active.cursor() >= self.active.len() {
            None
        } else {
            Some(self.active.alloc())
        }
    }

    #[inline]
    pub fn get(&self, idx: usize) -> &Cell {
        &self.active[idx]
    }

    #[inline]
    pub fn get_mut(&mut self, idx: usize) -> &mut Cell {
        &mut self.active[idx]
    }
}

impl core::ops::Index<CellRef> for Heap {
    type Output = Cell;
    #[inline]
    fn index(&self, idx: CellRef) -> &Self::Output {
        self.get(idx.0 as usize)
    }
}

impl core::ops::IndexMut<CellRef> for Heap {
    #[inline]
    fn index_mut(&mut self, idx: CellRef) -> &mut Self::Output {
        self.get_mut(idx.0 as usize)
    }
}

////////////////////////////////////////////////////////////////////////////////

#[repr(C)]
pub struct VM {
    heap: Heap,
    env: Env,
}

impl VM {
    pub fn new(heap_size: usize) -> Self {
        let mut primitives: [(&str, Cell); 3] = [
            ("nil", Cell::Nil),
            ("#f", Cell::Bool(false)),
            ("#t", Cell::Bool(true)),
        ];

        // 3 + 11 + 3 * 9 + 2 * 4 = 14 + 27 + 16 = 57
        let stdfuncs: [(&str, bool, &[&str], extern "C" fn(&mut VM) -> CellRef); 11] =
            [("+",      false, &["a", "b"], nat_add_int),
             ("-",      false, &["a", "b"], nat_sub_int),
             ("*",      false, &["a", "b"], nat_mul_int),
             ("/",      false, &["a", "b"], nat_div_int),
             ("=",      false, &["a", "b"], nat_eq),
             ("!=",     false, &["a", "b"], nat_neq),
             ("<",      false, &["a", "b"], nat_lt),
             (">",      false, &["a", "b"], nat_gt),
             ("if",     true,  &["b", "tbranch", "ebranch"],    nat_if),
             ("let",    true,  &["var", "value", "exp"],        nat_let),
             ("lambda", true,  &["args", "body"],               nat_lambda),
             ];

        let mut s = Self {
            heap: Heap::new(heap_size),
            env: Env::new(),
        };

        s.env.push_temp_frame();

        // default bindings
        for (n, c) in primitives.iter_mut() {
            let r = s.alloc();
            s[r] = ::core::mem::replace(c, Cell::Empty);
            s.push_binding(n, r);
        }

        assert!(s.allocated_cells() == 3);

        for (n, b, args, f) in stdfuncs.iter() {
            s.register_native_lambda(n, *b, args, *f);
        }
        s.env.pop_temp_frame();
        s
    }

    pub fn alloc(&mut self) -> CellRef {
        let r = match self.heap.alloc() {
            None => {
                self.heap.collect(&mut self.env);
                match self.heap.alloc() {
                    None => panic!("Out of memory!"),
                    Some(r) => r,
                }
            }
            Some(r) => r,
        };
        self.env.push_temp(r);
        r
    }

    pub fn list_to_array(&self, list: CellRef) -> Vec<CellRef> {
        let mut v = Vec::new();
        let mut c = list;
        let mut count = 0;
        loop {
            match self[c] {
                Cell::Nil => return v,
                Cell::Cons { head: h, tail: t } => {
                    v.push(h);
                    c = t;
                    count += 1
                }
                _ => panic!("Should only be Cons or Nil"),
            }

            if count >= self.heap.size() {
                panic!("Recursive list");
            }
        }
    }

    fn push_binding(&mut self, s: &str, v: CellRef) -> usize {
        self.env.push_binding(String::from(s), v)
    }

    pub fn eval_args(&mut self, args: CellRef, values: CellRef) {
        let args_ = self.list_to_array(args);
        let values_ = self.list_to_array(values);
        if args_.len() != values_.len() {
            panic!("Error: Lambda/Native argument count doesn't match passed values");
        }

        for i in 0..args_.len() {
            let v = self.eval(values_[i]);
            let s = match &self[args_[i]] {
                Cell::Symbol(s) => s.clone(),
                _ => panic!("Error: Lambda/Native should have symbols as arguments"),
            };
            self.push_binding(s.as_str(), v);
        }
    }

    pub fn bind_macro_args(&mut self, args: CellRef, values: CellRef) {
        let args_ = self.list_to_array(args);
        let values_ = self.list_to_array(values);
        if args_.len() != values_.len() {
            panic!("Error: Lambda/Native argument count doesn't match passed values");
        }

        for i in 0..args_.len() {
            let s = match &self[args_[i]] {
                Cell::Symbol(s) => s.clone(),
                _ => panic!("Error: Lambda/Native should have symbols as arguments"),
            };
            self.push_binding(s.as_str(), values_[i]);
        }
    }


    pub fn apply(&mut self, head: CellRef, tail: CellRef) -> CellRef {
        match self.heap[head] {
            Cell::Lambda { is_macro: m, args: a, body: b } => {
                //self.env.pushBindFrame();
                if m {
                    self.bind_macro_args(a, tail)
                } else {
                    self.eval_args(a, tail)
                }
                let res = self.eval(b);
                //self.env.popBindFrame();
                res
            },
            Cell::NativeLambda { is_macro: m, args: a, func: f } => {
                //self.env.pushBindFrame();
                if m {
                    self.bind_macro_args(a, tail)
                } else {
                    self.eval_args(a, tail)
                }
                let res = f(self);
                //self.env.popBindFrame();
                res
            },
            _ => panic!("Cannot apply this cell!"),
        }
    }

    pub fn eval(&mut self, cell: CellRef) -> CellRef {
        match self.heap[cell] {
            Cell::Cons { head: h, tail: t } => {
                let hh = self.eval(h);
                self.apply(hh, t)
            }
            Cell::Empty => panic!("Error: eval should not evaluate empty space!"),
            Cell::Forward(_) => panic!("Error: forward should not be evaluated!"), // Just for GC
            _ => {
                let v = match &self.heap[cell] {
                    Cell::Symbol(s) => Some(s),
                    _ => None,
                };
                match v {
                    None => cell,
                    Some(s) => {
                        self.env.value(s.as_str()).unwrap()
                    },
                }
            }
        }
    }

    pub fn register_native_lambda(&mut self, fname: &str, is_macro: bool, args: &[&str], native: extern "C" fn(&mut Self) -> CellRef) {
        self.env.push_temp_frame();
        let mut natref = self.alloc();
        self[natref] = Cell::Nil;

        if args.len() != 0 {
            for i in 0..args.len() {
                let n = self.alloc();
                self[n] = Cell::Symbol(String::from(args[args.len() - i - 1].clone()));
                let last = self.alloc();
                self[last] = Cell::Cons {
                    head: n,
                    tail: natref,
                };
                natref = last;
            }
        }

        let ncell = self.alloc();
        self[ncell] = Cell::NativeLambda {
            is_macro: is_macro,
            args: natref,
            func: native,
        };
        self.env.push_binding(String::from(fname), ncell);
        self.env.pop_temp_frame();
    }


    fn map_sexp(&mut self, exp: &Exp) -> CellRef {
        self.env.push_temp_frame();
        let mut cell = self.alloc();
        match exp {
            Exp::List(v) if v.len() == 0 => self[cell] = Cell::Nil,
            Exp::Int(i)     => self[cell] = Cell::Int(*i),
            Exp::Float(f)   => self[cell] = Cell::Float(*f),
            Exp::String(s)  => self[cell] = Cell::String(s.clone()),
            Exp::Symbol(s)  => self[cell] = Cell::Symbol(s.clone()),
            Exp::List(v)    => {
                self[cell] = Cell::Nil;
                for i in 0..v.len() {
                    let n = self.map_sexp(&v[v.len() - i - 1]);
                    let last = self.alloc();
                    self[last] = Cell::Cons {
                        head: n,
                        tail: cell,
                    };
                    cell = last;
                }
            }
        }
        self.env.pop_temp_frame();
        cell
    }

    pub fn run(&mut self, exp: &str) -> CellRef {
        let res = Exp::from_sexp(exp);
        let cell = match res {
            ParseResult::PRErr(_) => panic!("invalid exp"),
            ParseResult::PROk(res) => self.map_sexp(&res),
        };
        self.env.push_bind_frame();
        self.env.push_temp_frame();
        let r = self.eval(cell);
        self.env.pop_temp_frame();
        self.env.pop_bind_frame();
        r
    }

    pub fn get_value_of(&self, sym: &str) -> Option<CellRef> {
        self.env.value(sym)
    }

    pub fn env_symbols(&self) -> Vec<String> {
        let mut v = Vec::new();
        for i in 0..self.env.syms.len() {
            v.push(self.env.syms[i].var.clone());
        }
        v
    }
}

impl CellAllocator for VM {
    fn add(&mut self, cell: Cell) -> CellRef {
        let r = self.alloc();
        self.heap[r] = cell;
        r
    }

    fn get(&self, cref: CellRef) -> &Cell {
        &self.heap[cref]
    }

    fn gc(&mut self) {
        self.heap.collect(&mut self.env);
    }

    fn allocated_cells(&self) -> usize {
        self.heap.active.cursor()
    }
}

impl core::ops::Index<CellRef> for VM {
    type Output = Cell;
    #[inline]
    fn index(&self, idx: CellRef) -> &Self::Output {
        &self.heap[idx]
    }
}

impl core::ops::IndexMut<CellRef> for VM {
    #[inline]
    fn index_mut(&mut self, idx: CellRef) -> &mut Self::Output {
        &mut self.heap[idx]
    }
}

extern "C" fn nat_add_int(vm: &mut VM) -> CellRef {
    let a = vm.env.value("a").unwrap();
    let b = vm.env.value("b").unwrap();
    let r = (*vm).alloc();
    match (&vm[a], &vm[b]) {
        (Cell::Int(a), Cell::Int(b)) => {
            vm.heap[r] = Cell::Int(a + b);
            r
        }
        _ => panic!("+ expected both int"),
    }
}

extern "C" fn nat_sub_int(vm: &mut VM) -> CellRef {
    let a = vm.env.value("a").unwrap();
    let b = vm.env.value("b").unwrap();
    let r = (*vm).alloc();
    match (&vm[a], &vm[b]) {
        (Cell::Int(a), Cell::Int(b)) => {
            vm.heap[r] = Cell::Int(a - b);
            r
        }
        _ => panic!("- expected both int"),
    }
}

extern "C" fn nat_mul_int(vm: &mut VM) -> CellRef {
    let a = vm.env.value("a").unwrap();
    let b = vm.env.value("b").unwrap();
    let r = (*vm).alloc();
    match (&vm[a], &vm[b]) {
        (Cell::Int(a), Cell::Int(b)) => {
            vm.heap[r] = Cell::Int(a * b);
            r
        }
        _ => panic!("* expected both int"),
    }
}

extern "C" fn nat_div_int(vm: &mut VM) -> CellRef {
    let a = vm.env.value("a").unwrap();
    let b = vm.env.value("b").unwrap();
    let r = (*vm).alloc();
    match (&vm[a], &vm[b]) {
        (Cell::Int(a), Cell::Int(b)) => {
            vm.heap[r] = Cell::Int(a / b);
            r
        }
        _ => panic!("/ expected both int"),
    }
}

extern "C" fn nat_lt(vm: &mut VM) -> CellRef {
    let a = vm.env.value("a").unwrap();
    let b = vm.env.value("b").unwrap();
    let r = (*vm).alloc();
    match (&vm[a], &vm[b]) {
        (Cell::Int(a), Cell::Int(b)) => {
            vm.heap[r] = Cell::Bool(a < b);
            r
        }
        _ => panic!("< expected both int"),
    }
}

extern "C" fn nat_gt(vm: &mut VM) -> CellRef {
    let a = vm.env.value("a").unwrap();
    let b = vm.env.value("b").unwrap();
    let r = (*vm).alloc();
    match (&vm[a], &vm[b]) {
        (Cell::Int(a), Cell::Int(b)) => {
            vm.heap[r] = Cell::Bool(a > b);
            r
        }
        _ => panic!("> expected both int"),
    }
}


extern "C" fn nat_eq(vm: &mut VM) -> CellRef {
    let a = vm.env.value("a").unwrap();
    let b = vm.env.value("b").unwrap();
    let r = (*vm).alloc();
    match (&vm[a], &vm[b]) {
        (Cell::Int(a), Cell::Int(b))            => vm.heap[r] = Cell::Bool(a == b),
        (Cell::Bool(a), Cell::Bool(b))          => vm.heap[r] = Cell::Bool(a == b),
        (Cell::String(a), Cell::String(b))      => vm.heap[r] = Cell::Bool(a == b),
        (Cell::Symbol(a), Cell::Symbol(b))      => vm.heap[r] = Cell::Bool(a == b),
        _ => panic!("= expected both of the same type"),
    }
    r
}

extern "C" fn nat_neq(vm: &mut VM) -> CellRef {
    let a = vm.env.value("a").unwrap();
    let b = vm.env.value("b").unwrap();
    let r = (*vm).alloc();
    match (&vm[a], &vm[b]) {
        (Cell::Int(a), Cell::Int(b))            => vm.heap[r] = Cell::Bool(a != b),
        (Cell::Bool(a), Cell::Bool(b))          => vm.heap[r] = Cell::Bool(a != b),
        (Cell::String(a), Cell::String(b))      => vm.heap[r] = Cell::Bool(a != b),
        (Cell::Symbol(a), Cell::Symbol(b))      => vm.heap[r] = Cell::Bool(a != b),
        _ => panic!("= expected both of the same type"),
    }
    r
}

extern "C" fn nat_if(vm: &mut VM) -> CellRef {
    let b = vm.env.value("b").unwrap();
    let ibranch = vm.env.value("tbranch").unwrap();
    let ebranch = vm.env.value("ebranch").unwrap();
    let bv = vm.eval(b);
    match vm[bv] {
        Cell::Bool(b) => if b { vm.eval(ibranch) } else { vm.eval(ebranch) },
        _ => panic!("expecting bool")
    }
}

fn get_sym(vm: &VM, r: CellRef) -> String {
    match &vm[r] {
        Cell::Symbol(s) => s.clone(),
        _ => panic!("expected a symbol!")
    }
}

extern "C" fn nat_let(vm: &mut VM) -> CellRef {
    let value   = vm.env.value("value").unwrap();
    let bv = vm.eval(value);
    let var     = vm.env.value("var").unwrap();
    let sym     = get_sym(vm, var);
    vm.push_binding(sym.as_str(), bv);
    let exp     = vm.env.value("exp").unwrap();

    vm.eval(exp)
}

extern "C" fn nat_lambda(vm: &mut VM) -> CellRef {
    let args    = vm.env.value("args").unwrap();
    let body    = vm.env.value("body").unwrap();
    let v       = vm.alloc();
    vm[v]       = Cell::Lambda { args: args, is_macro: false, body: body };
    v
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eval_prim() {
        let mut vm = VM::new(512);
        let v = vm.run("123");
        assert!(vm[v].to_string(&vm) == "123");
    }

    #[test]
    fn test_eval_list() {
        let mut vm = VM::new(512);

        let v = vm.run("(+ 1234 8765)");
        assert!(vm[v].to_string(&vm) == "9999");

        let v = vm.run("(- 9999 8765)");
        assert!(vm[v].to_string(&vm) == "1234");

        // let v = vm.run("(* 4444 2)");
        // assert!(vm[v].toString(&vm) == "8888");

        // let v = vm.run("(/ 4444 2)");
        // assert!(vm[v].toString(&vm) == "2222");
    }

    #[test]
    fn test_eval_embedded_list() {
        let mut vm = VM::new(512);

        let v = vm.run("(+ (+ 1111 2222) (+ 3333 4444))");
        assert!(vm[v].to_string(&vm) == "11110");
    }

    #[test]
    fn test_conditions() {
        let mut vm = VM::new(512);

        let v = vm.run("(if (= (+ 1 2) 3) #t #f)");
        assert!(vm[v].to_string(&vm) == "#t");

        let v = vm.run("(if (= (+ 1 2) 4) #t #f)");
        assert!(vm[v].to_string(&vm) == "#f");
    }

    #[test]
    fn test_let() {
        let mut vm = VM::new(512);

        let v = vm.run("(let v 123 v)");
        assert!(vm[v].to_string(&vm) == "123");

        let v = vm.run("(let v (+ 123 432) v)");
        assert!(vm[v].to_string(&vm) == "555");
    }

    #[test]
    fn test_lambda() {
        let mut vm = VM::new(512);

        let v = vm.run("(let v (lambda (a b) (+ a b)) (v 123 543))");
        assert!(vm[v].to_string(&vm) == "666");
    }

    #[test]
    fn test_gc_simple() {
        let mut vm = VM::new(512);
        let initial_cell_count = vm.allocated_cells();
        vm.gc();
        let v = vm.run("(+ 0 123)");
        assert!(vm[v].to_string(&vm) == "123");

        let after_cell_count = vm.allocated_cells();
        assert!(after_cell_count > initial_cell_count);
        vm.gc();
        assert!(vm.allocated_cells() == initial_cell_count);
    }

    #[test]
    fn test_gc_complex() {
        let mut vm = VM::new(512);
        let initial_cell_count = vm.allocated_cells();
        vm.gc();

        let v = vm.run("(let v (lambda (a b) (+ a b)) (v 123 543))");
        assert!(vm[v].to_string(&vm) == "666");

        let after_cell_count = vm.allocated_cells();
        assert!(after_cell_count > initial_cell_count);
        vm.gc();

        assert!(vm.allocated_cells() == initial_cell_count);
    }
}
