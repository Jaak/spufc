module main;

import std.stdio  : writefln, writef;
import std.string : iswhite, split, tolower, atoi, format;
import std.file   : read;


// default to Stats version
version(NoStats) {} else{
  version = Stats;
}

// instruction code 
enum Instr {
    Mkbasic  , // no args
    Getbasic , 
    Eval     ,
    Add      ,
    Sub      ,
    Mul      ,
    Div      ,
    Leq      ,
    Eq       ,
    Apply    ,
    Update   ,
    Halt     ,
    Neg      ,
    Not      ,
    Mod      ,
    Neq      ,
    Le       ,
    Geq      ,
    Gr       ,
    Or       ,
    And      ,
    Nil      ,
    Cons     , // no args
    Loadc    , // arg1
    Pushloc  ,
    Pushglob ,
    Targ     ,
    Return   ,
    Slide    ,
    Alloc    ,
    Rewrite  ,
    Mkvec    ,
    Getvec   ,
    Get      ,
    Mkclos   ,
    Mkfunval ,
    Mark     ,
    Jumpz    ,
    Jump     ,
    Tlist    , // arg1
    Move     , // arg1, arg2
}

// instruction type
struct instr{
  Instr code;
  int   arg1;
  int   arg2;
  
  static instr opCall(Instr c, int i = 0, int o = 0){
    instr ins;
    ins.code = c;
    ins.arg1 = i;
    ins.arg2 = o;
    return ins;
  }
}

string show_instr(ref instr i){
  switch(i.code){
    case Instr.Mkbasic  : return format("mkbasic");  
    case Instr.Getbasic : return format("getbasic"); 
    case Instr.Eval     : return format("eval");     
    case Instr.Add      : return format("add");      
    case Instr.Sub      : return format("sub");      
    case Instr.Mul      : return format("mul");      
    case Instr.Div      : return format("div");      
    case Instr.Leq      : return format("leq");      
    case Instr.Eq       : return format("eq");       
    case Instr.Apply    : return format("apply");    
    case Instr.Update   : return format("update");   
    case Instr.Halt     : return format("halt");     
    case Instr.Neg      : return format("neg");      
    case Instr.Not      : return format("not");      
    case Instr.Mod      : return format("mod");      
    case Instr.Neq      : return format("neq");      
    case Instr.Le       : return format("le");       
    case Instr.Geq      : return format("geq");      
    case Instr.Gr       : return format("gr");       
    case Instr.Or       : return format("or");       
    case Instr.And      : return format("and");      
    case Instr.Nil      : return format("nil");      
    case Instr.Cons     : return format("cons");     
    case Instr.Loadc    : return format("loadc %d"   ,i.arg1);
    case Instr.Pushloc  : return format("pushloc %d" ,i.arg1);
    case Instr.Pushglob : return format("pushglob %d",i.arg1);
    case Instr.Targ     : return format("targ %d"    ,i.arg1);
    case Instr.Return   : return format("return %d"  ,i.arg1);
    case Instr.Slide    : return format("slide %d"   ,i.arg1);
    case Instr.Alloc    : return format("alloc %d"   ,i.arg1);
    case Instr.Rewrite  : return format("rewrite %d" ,i.arg1);
    case Instr.Mkvec    : return format("mkvec %d"   ,i.arg1);
    case Instr.Getvec   : return format("getvec %d"  ,i.arg1);
    case Instr.Get      : return format("get %d"     ,i.arg1);
    case Instr.Mkclos   : return format("mkclos %d"  ,i.arg1);
    case Instr.Mkfunval : return format("mkfunval %d",i.arg1);
    case Instr.Mark     : return format("mark %d"    ,i.arg1);
    case Instr.Jumpz    : return format("jumpz %d"   ,i.arg1);
    case Instr.Jump     : return format("jump %d"    ,i.arg1);
    case Instr.Tlist    : return format("tlist %d"   ,i.arg1);
    case Instr.Move     : return format("move %d %d" ,i.arg1,i.arg2);   
    default       : return format("<invalid>");
  }
}

void exec_instr(ref instr i, VM vm) in {
  assert(vm);
  assert(i.code >= Instr.min);
  assert(i.code <= Instr.max);
} body {
  switch(i.code){
    case Instr.Mkbasic  : mkbasic(vm)           ; version(Stats) ++vm.other ; return;  
    case Instr.Getbasic : getbasic(vm)          ; version(Stats) ++vm.other ; return;  
    case Instr.Eval     : eval(vm)              ; version(Stats) ++vm.other ; return;  
    case Instr.Add      : add(vm)               ; version(Stats) ++vm.basic_op ; return;  
    case Instr.Sub      : sub(vm)               ; version(Stats) ++vm.basic_op ; return;  
    case Instr.Mul      : mul(vm)               ; version(Stats) ++vm.basic_op ; return;  
    case Instr.Div      : div(vm)               ; version(Stats) ++vm.basic_op ; return;  
    case Instr.Leq      : leq(vm)               ; version(Stats) ++vm.basic_op ; return;  
    case Instr.Eq       : eq(vm)                ; version(Stats) ++vm.basic_op ; return;  
    case Instr.Apply    : apply(vm)             ; version(Stats) version(Stats) ++vm.other ; return;  
    case Instr.Update   : update(vm)            ; version(Stats) ++vm.other ; return;  
    case Instr.Halt     : halt(vm)              ; version(Stats) ++vm.other ; return;  
    case Instr.Neg      : neg(vm)               ; version(Stats) ++vm.basic_op ;return;  
    case Instr.Not      : not(vm)               ; version(Stats) ++vm.basic_op ;return;  
    case Instr.Mod      : mod(vm)               ; version(Stats) ++vm.basic_op ;return;  
    case Instr.Neq      : neq(vm)               ; version(Stats) ++vm.basic_op ;return;  
    case Instr.Le       : le(vm)                ; version(Stats) ++vm.basic_op ;return;  
    case Instr.Geq      : geq(vm)               ; version(Stats) ++vm.basic_op ;return;  
    case Instr.Gr       : gr(vm)                ; version(Stats) ++vm.basic_op ;return;  
    case Instr.Or       : or(vm)                ; version(Stats) ++vm.basic_op ;return;   
    case Instr.And      : and(vm)               ; version(Stats) ++vm.basic_op ;return;  
    case Instr.Nil      : nil(vm)               ; version(Stats) ++vm.other ; return;  
    case Instr.Cons     : cons(vm)              ; version(Stats) ++vm.other ; return;       
    case Instr.Loadc    : loadc(i.arg1,vm)      ; version(Stats) ++vm.other ; return;  
    case Instr.Pushloc  : pushloc(i.arg1,vm)    ; version(Stats) ++vm.other ; return;  
    case Instr.Pushglob : pushglob(i.arg1,vm)   ; version(Stats) ++vm.other ; return;  
    case Instr.Targ     : targ(i.arg1,vm)       ; version(Stats) ++vm.other ; return;  
    case Instr.Return   : ret(i.arg1,vm)        ; version(Stats) ++vm.other ; return;  
    case Instr.Slide    : slide(i.arg1,vm)      ; version(Stats) ++vm.other ; return;  
    case Instr.Alloc    : alloc(i.arg1,vm)      ; version(Stats) ++vm.other ; return;  
    case Instr.Rewrite  : rewrite(i.arg1,vm)    ; version(Stats) ++vm.other ; return;  
    case Instr.Mkvec    : mkvec(i.arg1,vm)      ; version(Stats) ++vm.other ; return;  
    case Instr.Getvec   : getvec(i.arg1,vm)     ; version(Stats) ++vm.other ; return;  
    case Instr.Get      : get(i.arg1,vm)        ; version(Stats) ++vm.other ; return;  
    case Instr.Mkclos   : mkclos(i.arg1,vm)     ; version(Stats) ++vm.other ; return;  
    case Instr.Mkfunval : mkfunval(i.arg1,vm)   ; version(Stats) ++vm.other ; return;  
    case Instr.Mark     : mark(i.arg1,vm)       ; version(Stats) ++vm.other ; return;  
    case Instr.Jumpz    : jumpz(i.arg1,vm)      ; version(Stats) ++vm.jumpzs; return;  
    case Instr.Jump     : jump(i.arg1,vm)       ; version(Stats) ++vm.jumps ; return;  
    case Instr.Tlist    : tlist(i.arg1,vm)      ; version(Stats) ++vm.other ; return;  
    case Instr.Move     : move(i.arg1,i.arg2,vm); version(Stats) ++vm.other ; return;  
  }
}

class VM {
  protected: 
    // Virtual machine state
    instr[] ins;  // instruction array
    int     pc;   // program counter
    
    Value[] stack; // stack state
    int     fp;    // frame pointer
    HObject gp;    // globals pinter
    
    bool    halted; // has machine stopped
  
  public:
    version(Stats){
    // statistics
    int jumps;     // number of unconditional jumps
    int jumpzs;    // number of conditional jumps
    int basic_op;  // number of arithmetic and logic ops
    int other;     // all other ops
    
    int objects;   // objects created
    int max_stack; // max stack size
    }
  public:
    this(){
      // vm state
      pc = 0;
      fp = -1;
      gp = null;
      halted = false;
      version(Stats) {
        // reset stats
        jumps = jumpzs = basic_op = other = 0;
        objects   = 0;
        max_stack = 0;
      }
    }
    
    this(string s){
      this();
      readMama(s);
    }
  
    // execute one step
    void step(){
      if (! halted){
        exec_instr(ins[pc++],this);
      }
    }

    // run program until it halts
    HObject run(int steps = -2){
      while (! halted && steps-- != 0){
        exec_instr(ins[pc++],this);
      }
      assert(stack.length == 1 && halted, "code did not finish properly" );
      return stack[0].addrData;
    }
    
    // parse mama file
    void readMama(string str)
    {
      instr [] res;
      string[] words;
      int[][string] jump_tbl;
      int  [string] lable_tbl;
      
      words = str.filterComments().tolower().split();
      
      while (words.length) {
        switch (words[0]) {
          /* normal 0-ary instructions */
          case "mkbasic"  : res ~= instr(Instr.Mkbasic )                 ; words = words[1..$]; break;
          case "getbasic" : res ~= instr(Instr.Getbasic)                 ; words = words[1..$]; break;
          case "eval"     : res ~= instr(Instr.Eval    )                 ; words = words[1..$]; break;
          case "add"      : res ~= instr(Instr.Add     )                 ; words = words[1..$]; break;
          case "sub"      : res ~= instr(Instr.Sub     )                 ; words = words[1..$]; break;
          case "mul"      : res ~= instr(Instr.Mul     )                 ; words = words[1..$]; break;
          case "div"      : res ~= instr(Instr.Div     )                 ; words = words[1..$]; break;
          case "leq"      : res ~= instr(Instr.Leq     )                 ; words = words[1..$]; break;
          case "eq"       : res ~= instr(Instr.Eq      )                 ; words = words[1..$]; break;
          case "apply"    : res ~= instr(Instr.Apply   )                 ; words = words[1..$]; break;
          case "update"   : res ~= instr(Instr.Update  )                 ; words = words[1..$]; break;
          case "halt"     : res ~= instr(Instr.Halt    )                 ; words = words[1..$]; break;
          case "neg"      : res ~= instr(Instr.Neg     )                 ; words = words[1..$]; break;
          case "not"      : res ~= instr(Instr.Not     )                 ; words = words[1..$]; break;
          case "mod"      : res ~= instr(Instr.Mod     )                 ; words = words[1..$]; break;
          case "neq"      : res ~= instr(Instr.Neq     )                 ; words = words[1..$]; break;
          case "le"       : res ~= instr(Instr.Le      )                 ; words = words[1..$]; break;
          case "geq"      : res ~= instr(Instr.Geq     )                 ; words = words[1..$]; break;
          case "gr"       : res ~= instr(Instr.Gr      )                 ; words = words[1..$]; break;
          case "or"       : res ~= instr(Instr.Or      )                 ; words = words[1..$]; break;
          case "and"      : res ~= instr(Instr.And     )                 ; words = words[1..$]; break;
          case "nil"      : res ~= instr(Instr.Nil     )                 ; words = words[1..$]; break;
          case "cons"     : res ~= instr(Instr.Cons    )                 ; words = words[1..$]; break;
          
          /* instructions with one constant arg. */
          case "loadc"    : res ~= instr(Instr.Loadc   ,atoi(words[1])) ; words = words[2..$]; break;
          case "pushloc"  : res ~= instr(Instr.Pushloc ,atoi(words[1])) ; words = words[2..$]; break;
          case "pushglob" : res ~= instr(Instr.Pushglob,atoi(words[1])) ; words = words[2..$]; break;
          case "targ"     : res ~= instr(Instr.Targ    ,atoi(words[1])) ; words = words[2..$]; break;
          case "return"   : res ~= instr(Instr.Return  ,atoi(words[1])) ; words = words[2..$]; break; 
          case "slide"    : res ~= instr(Instr.Slide   ,atoi(words[1])) ; words = words[2..$]; break;
          case "alloc"    : res ~= instr(Instr.Alloc   ,atoi(words[1])) ; words = words[2..$]; break;
          case "rewrite"  : res ~= instr(Instr.Rewrite ,atoi(words[1])) ; words = words[2..$]; break;
          case "mkvec"    : res ~= instr(Instr.Mkvec   ,atoi(words[1])) ; words = words[2..$]; break;
          case "getvec"   : res ~= instr(Instr.Getvec  ,atoi(words[1])) ; words = words[2..$]; break;
          case "get"      : res ~= instr(Instr.Get     ,atoi(words[1])) ; words = words[2..$]; break;
          
          /* instruction with two constant arguments*/
          case "move"     : res ~= instr(Instr.Move,atoi(words[1]),atoi(words[2]))  ; words = words[3..$]; break;
          
          /* instructions with possibly forward referencing lable args */
          case "mkclos"   : res ~= instr(Instr.Mkclos  )       ; jump_tbl[words[1]] ~= res.length-1; words = words[2..$]; break;
          case "mkfunval" : res ~= instr(Instr.Mkfunval)       ; jump_tbl[words[1]] ~= res.length-1; words = words[2..$]; break;
          case "mark"     : res ~= instr(Instr.Mark    )       ; jump_tbl[words[1]] ~= res.length-1; words = words[2..$]; break;
          case "tlist"    : res ~= instr(Instr.Tlist   )       ; jump_tbl[words[1]] ~= res.length-1; words = words[2..$]; break;
          case "jumpz"    : res ~= instr(Instr.Jumpz   )       ; jump_tbl[words[1]] ~= res.length-1; words = words[2..$]; break;
          case "jump"     : res ~= instr(Instr.Jump    )       ; jump_tbl[words[1]] ~= res.length-1; words = words[2..$]; break;
          
          /* lable or error */
          default:
            /* "lable:" case*/
            if (words[0][$-1] == ':'){
              assert(!(words[0][0..$-1] in lable_tbl));
              lable_tbl[words[0][0..$-1]] = res.length;
              words = words[1..$];
            /* "lable : " case */
            } else if (words.length > 1 && words[1] == ":"){
              assert(!(words[0] in lable_tbl));
              lable_tbl[words[0]] = res.length;
              words = words[2..$];
            /* "lable :<other>" case */
            } else if (words.length > 2 && words[1][0] == ':'){
              words[1] = words[1][1..$];
              assert(!(words[0] in lable_tbl));
              lable_tbl[words[0]] = res.length;
              words = words[1..$];
            /* could not parse */
            } else {
              throw new Error(format("parse error on: %s",words[0]));
              
            }
        }
      }
      
      // resolve references
      foreach(lable, instrs; jump_tbl){
        assert(lable in lable_tbl, format("lable %s undefined",lable));
        foreach(i;instrs){
          res[i].arg1 = lable_tbl[lable];
        }
      }
      
      // table is ready
      ins = res;
    }

    // state string
    string toString(){
      string str = format("PC = %d \t SP = %d \t FP = %d\nGP = %s \n STACK: \n",pc,stack.length-1,fp,gp);
      
      foreach(i,e;stack){
        str ~= format("%s\n",e);
        }
      
      return str;
    }

    // state string
    version(Stats) 
    string stats(){
      string fs = "Statistics: \njump: \t %d\t\tbasic_ops: \t%d\njumpz: \t %d\t\tother:  \t%d\nmax stack size:    \t%d \nheap objects created: \t%d";
      string str = format(fs,jumps,basic_op,jumpzs,other,max_stack,objects);
      
      return str;
    }
}

// somewhat-typed stack
struct Value {
  private:
    union{
      int     _intData;
      HObject _addrData;
    }
  
  public:
    enum : int {Int, Address};
    int type;
    
    static Value VInt(int i){
      Value v;
      v.type    = Int;
      v._intData = i;
      return v;
    }

    static Value VAddr(HObject i){
      Value v;
      v.type      = Address;
      v._addrData = i;
      return v;
    }
    
    int intData() in { 
      assert(type == Int);
    } body {
      return _intData;
    }

    HObject addrData() in { 
      assert(type == Address); 
    } body {
      return _addrData;
    }
    
    string toString(){    
      return type==Int ? format(intData) : format("@",addrData);
    }
    
    invariant{
      assert(type == Int || type == Address);
    }
}

// Heap Object class
class HObject {
  private:
    union{
      int _bv;
      struct{
        int     _ccp;
        HObject _cgp;
      }
      struct{
        int     _fcp;
        HObject _fap,_fgp;
      }
      struct{
        int       _vn;
        HObject[] _vv;
      }
      struct{
        HObject   _lhd;
        HObject   _ltl;
      }
    }
    
  public:
    enum : int {B, C, F, V, LNil,LCons};
    int type;
    
    // accessors that typecheck objects
    int       bv()  in { assert(type == B);     } body { return _bv;  }
    int       ccp() in { assert(type == C);     } body { return _ccp; }
    HObject   cgp() in { assert(type == C);     } body { return _cgp; }
    int       fcp() in { assert(type == F);     } body { return _fcp; }
    HObject   fap() in { assert(type == F);     } body { return _fap; }
    HObject   fgp() in { assert(type == F);     } body { return _fgp; }
    int       vn()  in { assert(type == V);     } body { return _vn;  }
    HObject[] vv()  in { assert(type == V);     } body { return _vv;  }
    HObject   lhd() in { assert(type == LCons); } body { return _lhd; }
    HObject   ltl() in { assert(type == LCons); } body { return _ltl; }

    this(){
      type = LNil;
    }

    this(int i){
      type = B;
      _bv = i;
    }

    this(int i, HObject o){
      type = C;
      _ccp  = i;
      _cgp  = o;
    }

    this(HObject i, HObject o){
      type = LCons;
      _lhd  = i;
      _ltl  = o;
    }

    this(int i, HObject o, HObject s){
      type = F;
      _fcp  = i;
      _fap  = o;
      _fgp  = s;
    }

    this(int i, HObject[] o){
      type = V;
      _vn   = i;
      _vv   = o;
    }
    
    void rewrite(HObject o){
      switch(type = o.type){
        case B: _bv = o.bv; break;
        case C: _ccp = o.ccp; _cgp = o.cgp; break;
        case F: _fcp = o.fcp; _fap = o.fap; _fgp = o.fgp; break;
        case V: _vn  = o.vn;  _vv  = o.vv ; break;
        case LNil: break;
        case LCons: _lhd = o.lhd; _ltl = o.ltl; break;
      }
    }
    
    string toString(){
      switch (type) {
        case B    : return format("%d",bv);
        case C    : return format("Closure(%s)",ccp);
        case F    : return format("Function(%s)",fcp);
        case V    : return format("Vector(%d,%s) ",vn,vv);
        case LNil : return format("Nil");
        case LCons: return format("%s : %s", lhd, ltl);
      }
    }
    
    invariant{
      assert(type == B || type == C || type == F || type == V || type == LNil || type == LCons);
    }
}


// instructions as functions -- hopefully compiler can inline some

void mkbasic(VM vm) {
  assert(vm.stack[$-1].type == Value.Int);
  vm.stack[$-1] = Value.VAddr(new HObject(vm.stack[$-1].intData));
  version(Stats) vm.objects++;
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}
unittest {
  assert((new VM("loadc 10 mkbasic halt")).run(3).bv == 10);
  assert((new VM("loadc 42 mkbasic halt")).run(3).bv == 42);
}

void getbasic(VM vm)
{
  assert(vm.stack[$-1].type          == Value.Address);
  assert(vm.stack[$-1].addrData.type == HObject.B);
  vm.stack[$-1] = Value.VInt(vm.stack[$-1].addrData.bv);
  version(Stats) vm.objects++;
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}
unittest {
  assert((new VM("loadc 32 loadc 10 mkbasic getbasic add mkbasic halt")).run(7).bv == 42);
}


void eval(VM vm) { 
  assert(vm.stack[$-1].type == Value.Address);
  if(vm.stack[$-1].addrData.type == HObject.C){
    // mark pc
    vm.stack ~= Value.VAddr(vm.gp);
    vm.stack ~= Value.VInt(vm.fp);
    vm.stack ~= Value.VInt(vm.pc);
    vm.fp = vm.stack.length-1;
    // pushlock 3
    assert(vm.stack[$-1-3].type == Value.Address);
    vm.stack ~= Value.VAddr(vm.stack[$-1-3].addrData);
    // apply0
    assert(vm.stack[$-1].type == Value.Address);
    assert(vm.stack[$-1].addrData.type == HObject.C);
    vm.gp = vm.stack[$-1].addrData.cgp;
    vm.pc = vm.stack[$-1].addrData.ccp;
    vm.stack = vm.stack[0..$-1];
    version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
  }
}

void add(VM vm) { 
  assert(vm.stack[$-1].type == Value.Int);
  assert(vm.stack[$-1-1].type == Value.Int);
  vm.stack[$-1-1] = Value.VInt(vm.stack[$-1-1].intData + vm.stack[$-1].intData);
  vm.stack = vm.stack[0..$-1];
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}


void sub(VM vm) { 
  assert(vm.stack[$-1].type == Value.Int);
  assert(vm.stack[$-1-1].type == Value.Int);
  vm.stack[$-1-1] = Value.VInt(vm.stack[$-1-1].intData - vm.stack[$-1].intData);
  vm.stack = vm.stack[0..$-1];
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}

void mul(VM vm) { 
  assert(vm.stack[$-1].type == Value.Int);
  assert(vm.stack[$-1-1].type == Value.Int);
  vm.stack[$-1-1] = Value.VInt(vm.stack[$-1-1].intData * vm.stack[$-1].intData);
  vm.stack = vm.stack[0..$-1];
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}

void div(VM vm) { 
  assert(vm.stack[$-1].type == Value.Int);
  assert(vm.stack[$-1-1].type == Value.Int);
  vm.stack[$-1-1] = Value.VInt(vm.stack[$-1-1].intData / vm.stack[$-1].intData);
  vm.stack = vm.stack[0..$-1];
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}

void leq(VM vm) { 
  assert(vm.stack[$-1].type == Value.Int);
  assert(vm.stack[$-1-1].type == Value.Int);
  vm.stack[$-1-1] = Value.VInt(vm.stack[$-1-1].intData <= vm.stack[$-1].intData);
  vm.stack = vm.stack[0..$-1];
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}

void eq(VM vm) { 
  assert(vm.stack[$-1].type == Value.Int);
  assert(vm.stack[$-1-1].type == Value.Int);
  vm.stack[$-1-1] = Value.VInt(vm.stack[$-1-1].intData == vm.stack[$-1].intData);
  vm.stack = vm.stack[0..$-1];
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}

void apply(VM vm) {
  assert(vm.stack[$-1].type == Value.Address);
  assert(vm.stack[$-1].addrData.type == HObject.F);
  assert(vm.stack[$-1].addrData.fap.type == HObject.V);
  HObject h = vm.stack[$-1].addrData;
  vm.stack = vm.stack[0..$-1];
  vm.gp = h.fgp;
  vm.pc = h.fcp;
  foreach(e;h.fap.vv){
    vm.stack ~= Value.VAddr(e);
  }
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}

void update(VM vm) { 
    auto fp = vm.fp ;
    
    assert(vm.stack[fp].type == Value.Int);
    vm.pc = vm.stack[fp].intData;

    assert(vm.stack[fp-2].type == Value.Address);
    vm.gp = vm.stack[fp-2].addrData;

    assert(vm.stack[fp-1].type == Value.Int);
    vm.fp = vm.stack[fp-1].intData;

    assert(vm.stack[$-1].type == Value.Address);
    assert(vm.stack[fp-3].type == Value.Address);
    vm.stack[fp-3].addrData.rewrite(vm.stack[$-1].addrData);

    vm.stack = vm.stack[0..fp-2];
    
    version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}

void halt(VM vm){ 
  vm.halted = true; 
}

void neg(VM vm) { 
  assert(vm.stack[$-1].type == Value.Int);
  vm.stack[$-1-1] = Value.VInt(- vm.stack[$-1].intData);
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}

void not(VM vm) { 
  assert(vm.stack[$-1].type == Value.Int);
  vm.stack[$-1-1] = Value.VInt(! vm.stack[$-1].intData);
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}

void mod(VM vm) { 
  assert(vm.stack[$-1].type == Value.Int);
  assert(vm.stack[$-1-1].type == Value.Int);
  vm.stack[$-1-1] = Value.VInt(vm.stack[$-1-1].intData % vm.stack[$-1].intData);
  vm.stack = vm.stack[0..$-1];
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}

void neq(VM vm) { 
  assert(vm.stack[$-1].type == Value.Int);
  assert(vm.stack[$-1-1].type == Value.Int);
  vm.stack[$-1-1] = Value.VInt(vm.stack[$-1-1].intData != vm.stack[$-1].intData);
  vm.stack = vm.stack[0..$-1];
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}

void le(VM vm) { 
  assert(vm.stack[$-1].type == Value.Int);
  assert(vm.stack[$-1-1].type == Value.Int);
  vm.stack[$-1-1] = Value.VInt(vm.stack[$-1-1].intData < vm.stack[$-1].intData);
  vm.stack = vm.stack[0..$-1];
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}

void geq(VM vm) { 
  assert(vm.stack[$-1].type == Value.Int);
  assert(vm.stack[$-1-1].type == Value.Int);
  vm.stack[$-1-1] = Value.VInt(vm.stack[$-1-1].intData >= vm.stack[$-1].intData);
  vm.stack = vm.stack[0..$-1];
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}

void gr(VM vm) { 
  assert(vm.stack[$-1].type == Value.Int);
  assert(vm.stack[$-1-1].type == Value.Int);
  vm.stack[$-1-1] = Value.VInt(vm.stack[$-1-1].intData > vm.stack[$-1].intData);
  vm.stack = vm.stack[0..$-1];
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}

void or(VM vm) { 
  assert(vm.stack[$-1].type == Value.Int);
  assert(vm.stack[$-1-1].type == Value.Int);
  vm.stack[$-1-1] = Value.VInt(vm.stack[$-1-1].intData || vm.stack[$-1].intData);
  vm.stack = vm.stack[0..$-1];
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}

void and(VM vm) { 
  assert(vm.stack[$-1].type == Value.Int);
  assert(vm.stack[$-1-1].type == Value.Int);
  vm.stack[$-1-1] = Value.VInt(vm.stack[$-1-1].intData && vm.stack[$-1].intData);
  vm.stack = vm.stack[0..$-1];
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}

void nil(VM vm) { 
  vm.stack ~= Value.VAddr(new HObject());
  version(Stats) vm.objects++;
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}

void cons(VM vm) { 
  assert(vm.stack[$-1].type == Value.Address);
  assert(vm.stack[$-2].type == Value.Address);
  vm.stack[$-2] = Value.VAddr(new HObject(vm.stack[$-2].addrData,vm.stack[$-1].addrData));
  vm.stack = vm.stack[0..$-1];
  version(Stats) vm.objects++;
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}



void loadc(int arg, VM vm) { 
  vm.stack ~= Value.VInt(arg);
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}

void pushloc(int arg, VM vm) { 
  assert(vm.stack[$-1-arg].type == Value.Address);
  vm.stack ~= Value.VAddr(vm.stack[$-1-arg].addrData);
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}

void pushglob(int arg, VM vm) { 
  assert(vm.gp.type == HObject.V);
  vm.stack ~= Value.VAddr(vm.gp.vv[arg]);
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}

void targ(int arg, VM vm) { 
  int g = vm.stack.length-1-vm.fp;
  auto fp = vm.fp;
  if (g < arg){
    // mkvec0
    HObject[] arr = new HObject[g]; 
    foreach(i,e;vm.stack[$-g..$].reverse){
      assert(e.type == Value.Address);
      arr[i] = e.addrData;
    }
    auto ap = new HObject(g,arr);

    //  wrap A
    HObject f = new HObject(vm.pc-1,ap,vm.gp);

    // popenv      

    assert(vm.stack[fp-2].type == Value.Address);
    vm.gp = vm.stack[fp-2].addrData;
    vm.stack[fp-2] = Value.VAddr(f); 

    assert(vm.stack[fp].type == Value.Int);
    vm.pc = vm.stack[fp].intData;

    assert(vm.stack[fp-1].type == Value.Int);
    vm.fp = vm.stack[fp-1].intData;

    vm.stack = vm.stack[0..fp-1];
    version(Stats) vm.objects+=3;
    version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
  }
}

void ret(int arg, VM vm) { 
  auto sp = vm.stack.length-1;
  auto fp = vm.fp;
  if (sp - fp == arg + 1){
    // popenv
    assert(vm.stack[fp-2].type == Value.Address);
    vm.gp = vm.stack[fp-2].addrData;      
    
    vm.stack[fp-2] = vm.stack[$-1] ;
    
    assert(vm.stack[fp].type == Value.Int);
    vm.pc = vm.stack[fp].intData;
          
    assert(vm.stack[fp-1].type == Value.Int);
    vm.fp = vm.stack[fp-1].intData;
    
    vm.stack = vm.stack[0..fp-1];
  } else {
    // slide k
    vm.stack[$-1-arg] = vm.stack[$-1];
    vm.stack = vm.stack[0..$-arg];
    // apply
    assert(vm.stack[$-1].type == Value.Address);
    assert(vm.stack[$-1].addrData.type == HObject.F);
    assert(vm.stack[$-1].addrData.fap.type == HObject.V);
    HObject h = vm.stack[$-1].addrData;
    vm.stack = vm.stack[0..$-1];
    vm.gp = h.fgp;
    vm.pc = h.fcp;
    foreach(e;h.fap.vv){
      vm.stack ~= Value.VAddr(e);
    }
  }
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}

void slide(int arg, VM vm) { 
  vm.stack[$-1-arg] = vm.stack[$-1];
  vm.stack = vm.stack[0..$-arg];
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}

void alloc(int arg, VM vm) { 
  for(int i = 0; i < arg; ++i){
    vm.stack ~= Value.VAddr(new HObject(-1,cast(HObject)null));
    version(Stats) vm.objects++;
  }
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}

void rewrite(int arg, VM vm) { 
  assert(vm.stack[$-1-arg].type == Value.Address);
  assert(vm.stack[$-1].type == Value.Address);
  vm.stack[$-1-arg].addrData.rewrite(vm.stack[$-1].addrData);
  vm.stack = vm.stack[0..$-1];
  version(Stats)  if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}

void mkvec(int arg, VM vm) {
  HObject[] arr;
  foreach(e;vm.stack[$-arg..$]){
    assert(e.type == Value.Address);
    arr ~= e.addrData;
  }
  vm.stack = vm.stack[0..$-arg];
  vm.stack ~= Value.VAddr(new HObject(arg,arr));
  version(Stats) vm.objects++;
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}

void getvec(int arg, VM vm) { 
  assert(vm.stack[$-1].type == Value.Address);
  assert(vm.stack[$-1].addrData.type == HObject.V );
  HObject o = vm.stack[$-1].addrData;
  vm.stack = vm.stack[0..$-1];
  foreach(e;o.vv)
    vm.stack ~= Value.VAddr(e);
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}

void get(int arg, VM vm) { 
  assert(vm.stack[$-1].type == Value.Address);
  assert(vm.stack[$-1].addrData.type == HObject.V);
  vm.stack[$-1] = Value.VAddr(vm.stack[$-1].addrData.vv[arg]);
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}


void mkclos(int arg, VM vm) { 
  assert(vm.stack[$-1].type == Value.Address);
  assert(vm.stack[$-1].addrData.type == HObject.V);
  vm.stack[$-1] = Value.VAddr(new HObject(arg,vm.stack[$-1].addrData));
  version(Stats) vm.objects++;
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}

void mkfunval(int arg, VM vm) { 
  assert(vm.stack[$-1].type == Value.Address);
  assert(vm.stack[$-1].addrData.type == HObject.V);
  HObject h = new HObject(0,[]);
  vm.stack[$-1] = Value.VAddr(new HObject(arg,h,vm.stack[$-1].addrData));
  version(Stats) vm.objects++;
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}

void mark(int arg, VM vm) { 
  vm.stack ~= Value.VAddr(vm.gp);
  vm.stack ~= Value.VInt(vm.fp);
  vm.stack ~= Value.VInt(arg);
  vm.fp = vm.stack.length-1;
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}

void jumpz(int arg, VM vm) { 
  assert(vm.stack[$-1].type == Value.Int);
  if (vm.stack[$-1].intData == 0)
    vm.pc = arg;
  vm.stack = vm.stack[0..$-1];
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}

void jump(int arg, VM vm) { 
    vm.pc = arg;
    version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}

void tlist(int arg, VM vm) { 
  assert(vm.stack[$-1].type == Value.Address);
  
  if (vm.stack[$-1].addrData.type == HObject.LNil) {
    vm.stack = vm.stack[0..$-1];
  } else if (vm.stack[$-1].addrData.type == HObject.LCons) {
    auto list = vm.stack[$-1].addrData;
    vm.stack[$-1]  = Value.VAddr(list.lhd);
    vm.stack      ~= Value.VAddr(list.ltl);
    vm.pc = arg;
  } else throw new Error("tlist: not given a list");
  
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}

void move(int r, int k, VM vm) { 
  foreach(i,e;vm.stack[$-k..$]){
    vm.stack[$-k-r+i] = e;
  }
  vm.stack = vm.stack[0..$-r];
  
  version(Stats) if (vm.max_stack < vm.stack.length) vm.max_stack = vm.stack.length;
}




/* string processing  */

// drop all whitespace from front
string dropWhite(string str){
  size_t i = 0;
  for(;str.length > i && iswhite(str[i]);++i) {};
  return str[i..$];
}


string takeUntil(string str, dchar d)
{
  int v = str.length;
  foreach(i,c;str){
    if (c == d){
      v = i;
      break;
    }
  }
  return str[0..v];
}


string filterComments(string str){
  string done;
  string cur;
  
  // filter out she-bang
  if (str[0..2] == "#!"){
    auto fl = takeUntil(str,'\n');
    str = str[fl.length..$];
  }
  
  // filter out /* */-comments
  while(str.length){
    cur = takeUntil(str,'/');
    done ~= cur ;
    str = str[cur.length .. $];

    if (str.length < 2) {
      done ~= str;
      break;
    }
    
    if (str[1] == '*'){
      str = str[2..$];
    } else {
      done ~= "/" ;
      str = str[1..$];
      continue;
    }
    
    
    cur = takeUntil(str,'*');
    str = str[cur.length .. $];
    
    if (str.length < 2) break;
    
    while (str[1] != '/'){
      cur = takeUntil(str,'*');
      str = str[cur.length  .. $];
      
      if (str.length < 2) break;
    }
    
    if (str.length < 2) break;
    str = str[2..$];
  }
  
  return done;
}

// boolean to identify if unittests are working
bool unittestsWorking = false;
unittest{
  unittestsWorking = true;
}

// prints usage informations
void printUsage(){
    writefln("Usage: mama [options] <file.cbn> ");  
    writefln("Options: -v       \t print state and instruction on every step");  
    writefln("         -i       \t print instruction for every step");  
    writefln("         -s       \t after execution, print statistics ");  
    writefln("         -r       \t only print result (overrides all previous) ");  
    writefln("         -?       \t this text ");  
    writefln("         -steps n \t execute at most n steps");  
    writefln();  
    writefln("Built on %s using %s",__TIMESTAMP__,__VENDOR__);  
    
    try {
      int x;
      assert(x == 0 && x == 1 );
      writefln("Warning: built in 'release' mode so 'type' checks are disabled. ");
    } catch  {}
    
    if (!unittestsWorking)
      writefln("Warning: built without unittests enabled -- nothing might work ");    
}

int main(string[] args)
{
  // usage information
  if (args.length < 2){
    printUsage();
    return 0;
  }
  
  // all inputs -- will be read from cl arguments
  string file;
  long   run_steps = -1;
  bool   states    = false;
  bool   instr     = false;
  bool   stats     = false;
  bool   result    = false;
  
  // process arguments
  for(int i = 1; i < args.length; ++i){
    if (args[i] == "-steps") { 
      run_steps = atoi(args[i+1]);
      i++;
    } else if (args[i] == "-v") {
      states = true;
      instr  = true;
    } else if (args[i] == "-i") {
      instr  = true;
    } else if (args[i] == "-s") {
      stats  = true;
    } else if (args[i] == "-r") {
      result = true;
    } else if (args[i] == "-?") {
      printUsage();
      return 0;
    } else {
      if (file.length)
        throw new Error(format("unexpected argument: %s",args[i]));
      file = args[i];
    }
  }
  
  // create vm
  VM v1 = new VM;
  v1.readMama(cast(char[])read(file));
  
  // run programs
  int step = 0;
  while (!v1.halted && (run_steps < 0 || step < run_steps)){
    step++;
    
    // current instruction
    if (!result && instr) {
      writefln("%d:\tpc:%d\tinstr: %s",step, v1.pc, show_instr(v1.ins[v1.pc]));
    }
    
    // state before step
    if (!result && states){
      writefln(v1);
    }
    
    // one step
    v1.step();
  }
  
  if (result) {
    // print short result
    if (v1.halted)
      writefln(v1.stack[0].addrData);
    else
      writefln("<program did not halt in %d steps>",step);
  } else {
    // print long result
    if (v1.halted){
      writefln("program halted after %d steps", step);
    } else {
      writefln("program state after %d steps", step);
    }
    writefln(v1);
  }
  
  version(Stats) 
  if (stats){
    writefln("----------\n",v1.stats(),"\n","----------");
  }

  return 0;
}