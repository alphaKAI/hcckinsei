namespace HccFS

module KinseiCompiler =
    type Register =
        | R0
        | R1
        | R2
        | R3
        | R4
        | R5
        | R6
        | R7
        | R8
        | R9
        | R10
        | R11
        | R12
        | R13
        | R14
        | R15

        override x.ToString() =
            match x with
            | R0 -> "r0"
            | R1 -> "r1"
            | R2 -> "r2"
            | R3 -> "r3"
            | R4 -> "r4"
            | R5 -> "r5"
            | R6 -> "r6"
            | R7 -> "r7"
            | R8 -> "r8"
            | R9 -> "r9"
            | R10 -> "r10"
            | R11 -> "r11"
            | R12 -> "r12"
            | R13 -> "r13"
            | R14 -> "r14"
            | R15 -> "r15"

        static member FromString s =
            match s with
            | "r0" -> R0
            | "r1" -> R1
            | "r2" -> R2
            | "r3" -> R3
            | "r4" -> R4
            | "r5" -> R5
            | "r6" -> R6
            | "r7" -> R7
            | "r8" -> R8
            | "r9" -> R9
            | "r10" -> R10
            | "r11" -> R11
            | "r12" -> R12
            | "r13" -> R13
            | "r14" -> R14
            | "r15" -> R15
            | _ -> failwithf "No such a Register"

        member x.ToByte() =
            match x with
            | R0 -> 0
            | R1 -> 1
            | R2 -> 2
            | R3 -> 3
            | R4 -> 4
            | R5 -> 5
            | R6 -> 6
            | R7 -> 7
            | R8 -> 8
            | R9 -> 9
            | R10 -> 10
            | R11 -> 11
            | R12 -> 12
            | R13 -> 13
            | R14 -> 14
            | R15 -> 15

    type KinseiOpCode =
        | AddI of Register * int16
        | AddR of Register * Register
        | SubI of Register * int16
        | SubR of Register * Register
        | MulI of Register * int16
        | MulR of Register * Register
        | DivI of Register * int16
        | DivR of Register * Register
        | CmpI of Register * int16
        | CmpR of Register * Register
        | AbsI of Register * int16
        | AbsR of Register * Register
        | AdcI of Register * int16
        | AdcR of Register * Register
        | SbcI of Register * int16
        | SbcR of Register * Register
        | ShlI of Register * int16
        | ShlR of Register * Register
        | ShrI of Register * int16
        | ShrR of Register * Register
        | AshI of Register * int16
        | AshR of Register * Register
        | RolI of Register * int16
        | RolR of Register * Register
        | RorI of Register * int16
        | RorR of Register * Register
        | And of Register * Register
        | Or of Register * Register
        | Not of Register * Register
        | Xor of Register * Register
        | SetL of Register * int16
        | SetH of Register * int16
        | Load of Register * Register * int16
        | Store of Register * int16 * Register
        | JmpI of int16
        | JmpR of Register
        | JzI of int16
        | JzR of Register
        | JpI of int16
        | JpR of Register
        | JnI of int16
        | JnR of Register
        | JcI of int16
        | JcR of Register
        | JoI of int16
        | JoR of Register
        | JmpaI of int16
        | JmpaR of Register
        | JazI of int16
        | JazR of Register
        | JapI of int16
        | JapR of Register
        | JanI of int16
        | JanR of Register
        | JacI of int16
        | JacR of Register
        | JaoI of int16
        | JaoR of Register
        | Nop
        | Hlt

        member x.ToByte() =
            match x with
            | AddI _
            | AddR _ -> 0b000_0000
            | SubI _
            | SubR _ -> 0b000_0001
            | MulI _
            | MulR _ -> 0b000_0010
            | DivI _
            | DivR _ -> 0b000_0011
            | CmpI _
            | CmpR _ -> 0b000_0100
            | AbsI _
            | AbsR _ -> 0b000_0101
            | AdcI _
            | AdcR _ -> 0b000_0110
            | SbcI _
            | SbcR _ -> 0b000_0111
            | ShlI _
            | ShlR _ -> 0b000_1000
            | ShrI _
            | ShrR _ -> 0b000_1001
            | AshI _
            | AshR _ -> 0b000_1010
            | RolI _
            | RolR _ -> 0b000_1100
            | RorI _
            | RorR _ -> 0b000_1101
            | And _ -> 0b001_0000
            | Or _ -> 0b001_0001
            | Not _ -> 0b001_0010
            | Xor _ -> 0b001_0011
            | SetL _ -> 0b001_0110
            | SetH _ -> 0b001_0111
            | Load _ -> 0b001_1000
            | Store _ -> 0b001_1001
            | JmpI _
            | JmpR _
            | JzI _
            | JzR _
            | JpI _
            | JpR _
            | JnI _
            | JnR _
            | JcI _
            | JcR _
            | JoI _
            | JoR _ -> 0b001_1100
            | JmpaI _
            | JmpaR _
            | JazI _
            | JazR _
            | JapI _
            | JapR _
            | JanI _
            | JanR _
            | JacI _
            | JacR _
            | JaoI _
            | JaoR _ -> 0b001_1101
            | Nop -> 0b001_1110
            | Hlt -> 0b001_1111

         override x.ToString() =
            match x with
            | AddI (r, i) -> sprintf "addi %s, %d" (r.ToString()) i
            | AddR (r1, r2) -> sprintf "addr %s, %s" (r1.ToString()) (r2.ToString())
            | SubI (r, i) -> sprintf "subi %s, %d" (r.ToString()) i
            | SubR (r1, r2) -> sprintf "subr %s, %s" (r1.ToString()) (r2.ToString())
            | MulI (r, i) -> sprintf "muli %s, %d" (r.ToString()) i
            | MulR (r1, r2) -> sprintf "mulr %s, %s" (r1.ToString()) (r2.ToString())
            | DivI (r, i) -> sprintf "divi %s, %d" (r.ToString()) i
            | DivR (r1, r2) -> sprintf "divr %s, %s" (r1.ToString()) (r2.ToString())
            | CmpI (r, i) -> sprintf "cmpi %s, %d" (r.ToString()) i
            | CmpR (r1, r2) -> sprintf "cmpr %s, %s" (r1.ToString()) (r2.ToString())
            | AbsI (r, i) -> sprintf "absi %s, %d" (r.ToString()) i
            | AbsR (r1, r2) -> sprintf "absr %s, %s" (r1.ToString()) (r2.ToString())
            | AdcI (r, i) -> sprintf "adci %s, %d" (r.ToString()) i
            | AdcR (r1, r2) -> sprintf "adcr %s, %s" (r1.ToString()) (r2.ToString())
            | SbcI (r, i) -> sprintf "sbci %s, %d" (r.ToString()) i
            | SbcR (r1, r2) -> sprintf "sbcr %s, %s" (r1.ToString()) (r2.ToString())
            | ShlI (r, i) -> sprintf "shli %s, %d" (r.ToString()) i
            | ShlR (r1, r2) -> sprintf "shlr %s, %s" (r1.ToString()) (r2.ToString())
            | ShrI (r, i) -> sprintf "shri %s, %d" (r.ToString()) i
            | ShrR (r1, r2) -> sprintf "shrr %s, %s" (r1.ToString()) (r2.ToString())
            | AshI (r, i) -> sprintf "ashi %s, %d" (r.ToString()) i
            | AshR (r1, r2) -> sprintf "ashr %s, %s" (r1.ToString()) (r2.ToString())
            | RolI (r, i) -> sprintf "roli %s, %d" (r.ToString()) i
            | RolR (r1, r2) -> sprintf "rolr %s, %s" (r1.ToString()) (r2.ToString())
            | RorI (r, i) -> sprintf "rori %s, %d" (r.ToString()) i
            | RorR (r1, r2) -> sprintf "rorr %s, %s" (r1.ToString()) (r2.ToString())
            | And (r1, r2) -> sprintf "and %s, %s" (r1.ToString()) (r2.ToString())
            | Or  (r1, r2) -> sprintf "or %s, %s" (r1.ToString()) (r2.ToString())
            | Not (r1, r2) -> sprintf "not %s, %s" (r1.ToString()) (r2.ToString())
            | Xor (r1, r2) -> sprintf "xor %s, %s" (r1.ToString()) (r2.ToString())
            | SetL (r, i) -> sprintf "setl %s, %d" (r.ToString()) i
            | SetH (r, i) -> sprintf "setl %s, %d" (r.ToString()) i
            | Load (r1, r2, i) -> sprintf "load %s, %s, %d" (r1.ToString()) (r2.ToString()) i
            | Store (r1, i, r2) -> sprintf "load %s, %d, %s" (r1.ToString()) i (r2.ToString())
            | JmpI i -> sprintf "jmpi %d" i
            | JmpR r -> sprintf "jmpr %s" (r.ToString())
            | JzI i -> sprintf "jzi %d" i
            | JzR r -> sprintf "jzr %s" (r.ToString())
            | JpI i -> sprintf "jpi %d" i
            | JpR r -> sprintf "jpr %s" (r.ToString())
            | JnI i -> sprintf "jni %d" i
            | JnR r -> sprintf "jnr %s" (r.ToString())
            | JcI i -> sprintf "jci %d" i
            | JcR r -> sprintf "jcr %s" (r.ToString())
            | JoI i -> sprintf "joi %d" i
            | JoR r -> sprintf "jor %s" (r.ToString())
            | JmpaI i -> sprintf "jmpai %d" i
            | JmpaR r -> sprintf "jmpar %s" (r.ToString())
            | JazI i -> sprintf "jazi %d" i
            | JazR r -> sprintf "jazr %s" (r.ToString())
            | JapI i -> sprintf "japi %d" i
            | JapR r -> sprintf "japr %s" (r.ToString())
            | JanI i -> sprintf "jani %d" i
            | JanR r -> sprintf "janr %s" (r.ToString())
            | JacI i -> sprintf "jaci %d" i
            | JacR r -> sprintf "jacr %s" (r.ToString())
            | JaoI i -> sprintf "jaoi %d" i
            | JaoR r -> sprintf "jaor %s" (r.ToString())
            | Nop -> "nop"
            | Hlt -> "hlt"


    open OpCode
    open Ksexp

    exception UnimplementedException
    let unimplemented () = raise UnimplementedException

    let mutable sp = -1
    let incr () = sp <- sp + 1
    let decr () = sp <- sp - 1
    let getReg () =
        if 0 <= sp && sp <= 15 then
            let rs = sprintf "r%d" sp
            Register.FromString rs
        else
            failwithf "Out of range (sp: %A)" sp

    let CompileOpToKinseiCode op =
        let movi reg i = [Xor(reg, reg); AddI(reg, i)]

        match op with
        | OpPop -> unimplemented ()
        | OpPush vmv ->
            match vmv with
            | VValue sobj ->
                match sobj with
                | Float f ->
                    let i = int16 f
                    incr ()
                    let reg = getReg ()
                    movi reg i
                | Bool b ->
                    let i = if b then int16 1 else int16 0
                    incr ()
                    let reg = getReg ()
                    movi reg i
                | _ -> unimplemented()
            | VFunc _ -> unimplemented ()
        | OpAllocLvars _ -> unimplemented ()
        | OpFreeVars -> unimplemented ()
        | OpGetLocal idx -> unimplemented ()
        | OpSetLocal _ -> unimplemented ()
        | OpSetArgLocal _ -> unimplemented ()
        | OpAdd ->
            let r1 = getReg()
            decr()
            let r2 = getReg()
            [AddR(r2, r1)]
        | OpSub ->
            let r1 = getReg()
            decr()
            let r2 = getReg()
            [SubR(r2, r1)]
        | OpMul ->
            let r1 = getReg()
            decr()
            let r2 = getReg()
            [MulR(r2, r1)]
        | OpDiv ->
            let r1 = getReg()
            decr()
            let r2 = getReg()
            [DivR(r2, r1)]
        | OpMod
        | OpEq
        | OpNeq
        | OpLt
        | OpLeq
        | OpGt
        | OpGeq _ -> unimplemented ()
        | OpPrint
        | OpPrintln -> unimplemented ()
        | OpJumpRel _
        | OpFuncDef _ -> unimplemented ()
        | OpCall _
        | OpReturn -> unimplemented ()
        | OpVarDef _
        | OpGetVar _
        | OpSetVar _
        | OpBranch _
        | OpMakeList _
        | OpSetArgFrom _
        | OpDumpEnv _ -> unimplemented ()

    let CompileToKinseiCode code =
        List.collect (fun op ->
            let r = CompileOpToKinseiCode op
            printfn "op: %A, r: %A" op r
            r) code
