module main;

import std.stdio  : writefln, writef;
import std.string : iswhite, split, tolower, atoi, format;
import std.file   : read;

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

void exec_instr(ref instr i, VM vm){
  switch(i.code){
    case Instr.Mkbasic  : mkbasic(vm)           ; return;  
    case Instr.Getbasic : getbasic(vm)          ; return;  
    case Instr.Eval     : eval(vm)              ; return;  
    case Instr.Add      : add(vm)               ; return;  
    case Instr.Sub      : sub(vm)               ; return;  
    case Instr.Mul      : mul(vm)               ; return;  
    case Instr.Div      : div(vm)               ; return;  
    case Instr.Leq      : leq(vm)               ; return;  
    case Instr.Eq       : eq(vm)                ; return;  
    case Instr.Apply    : apply(vm)             ; return;  
    case Instr.Update   : update(vm)            ; return;  
    case Instr.Halt     : halt(vm)              ; return;  
    case Instr.Neg      : neg(vm)               ; return;  
    case Instr.Not      : not(vm)               ; return;  
    case Instr.Mod      : mod(vm)               ; return;  
    case Instr.Neq      : neq(vm)               ; return;  
    case Instr.Le       : le(vm)                ; return;  
    case Instr.Geq      : geq(vm)               ; return;  
    case Instr.Gr       : gr(vm)                ; return;  
    case Instr.Or       : or(vm)                ; return;   
    case Instr.And      : and(vm)               ; return;  
    case Instr.Nil      : nil(vm)               ; return;  
    case Instr.Cons     : cons(vm)              ; return;       
    case Instr.Loadc    : loadc(i.arg1,vm)      ; return;  
    case Instr.Pushloc  : pushloc(i.arg1,vm)    ; return;  
    case Instr.Pushglob : pushglob(i.arg1,vm)   ; return;  
    case Instr.Targ     : targ(i.arg1,vm)       ; return;  
    case Instr.Return   : ret(i.arg1,vm)        ; return;  
    case Instr.Slide    : slide(i.arg1,vm)      ; return;  
    case Instr.Alloc    : alloc(i.arg1,vm)      ; return;  
    case Instr.Rewrite  : rewrite(i.arg1,vm)    ; return;  
    case Instr.Mkvec    : mkvec(i.arg1,vm)      ; return;  
    case Instr.Getvec   : getvec(i.arg1,vm)     ; return;  
    case Instr.Get      : get(i.arg1,vm)        ; return;  
    case Instr.Mkclos   : mkclos(i.arg1,vm)     ; return;  
    case Instr.Mkfunval : mkfunval(i.arg1,vm)   ; return;  
    case Instr.Mark     : mark(i.arg1,vm)       ; return;  
    case Instr.Jumpz    : jumpz(i.arg1,vm)      ; return;  
    case Instr.Jump     : jump(i.arg1,vm)       ; return;  
    case Instr.Tlist    : tlist(i.arg1,vm)      ; return;  
    case Instr.Move     : move(i.arg1,i.arg2,vm); return;  
  }
}

class VM {
  protected:
    instr[] ins;
    int     pc;
    
    Value[] stack;
    int     fp;
    HObject gp;
    
    bool    halted;
  public:
    this(){
      pc = 0;
      fp = -1;
      gp = null;
      halted = false;
    }
  
    void step(){
      if (! halted){
        exec_instr(ins[pc++],this);
      }
    }

    void run(){
      while (! halted){
        exec_instr(ins[pc++],this);
      }
    }
    
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
            } else if (words.length && words[1] == ":"){
              assert(!(words[0] in lable_tbl));
              lable_tbl[words[0]] = res.length;
              words = words[2..$];
            /* could not parse */
            } else {
              throw new Error(format("parse error on: %s",words[0]));
              
            }
        }
      }
      
      // resolve references
      foreach(lable, addr; lable_tbl){
        foreach(i;jump_tbl[lable]){
          res[i].arg1 = addr;
        }
      }
      
      // table is ready
      ins = res;
    }


    string toString(){
      string str = format("PC = %d \t SP = %d \t FP = %d\n GP = %s \n STACK: \n",pc,stack.length-1,fp,gp);
      
      foreach(i,e;stack){
        str ~= format("%s\n",e);
        }
      
      return str;
    }
}

struct Value {
  enum : int {Int, Address};
  int type;
  union{
    int     intData;
    HObject addrData;
  }
  
  static Value VInt(int i){
    Value v;
    v.type    = Int;
    v.intData = i;
    return v;
  }

  static Value VAddr(HObject i){
    Value v;
    v.type     = Address;
    v.addrData = i;
    return v;
  }
  
  string toString(){    
    return type==Int ? format(intData) : format("@",addrData);
  }
  
}

class HObject {
  public:
    enum : int {B, C, F, V, LNil,LCons};
    int type;
    union{
      int bv;
      struct{
        int     ccp;
        HObject cgp;
      }
      struct{
        int     fcp;
        HObject fap,fgp;
      }
      struct{
        int       vn;
        HObject[] vv;
      }
      struct{
        HObject   lhd;
        HObject   ltl;
      }
    }
    
    this(){
      type = LNil;
    }

    this(int i){
      type = B;
      bv = i;
    }

    this(int i, HObject o){
      type = C;
      ccp  = i;
      cgp  = o;
    }

    this(HObject i, HObject o){
      type = LCons;
      lhd  = i;
      ltl  = o;
    }

    this(int i, HObject o, HObject s){
      type = F;
      fcp  = i;
      fap  = o;
      fgp  = s;
    }

    this(int i, HObject[] o){
      type = V;
      vn   = i;
      vv   = o;
    }
    
    void rewrite(HObject o){
      switch(type = o.type){
        case B: bv = o.bv; break;
        case C: ccp = o.ccp; cgp = o.cgp; break;
        case F: fcp = o.fcp; fap = o.fap; fgp = o.fgp; break;
        case V: vn  = o.vn;  vv  = o.vv ; break;
        case LNil: break;
        case LCons: lhd = o.lhd; ltl = o.ltl; break;
      }
    }
    
    string toString(){
      switch (type) {
        case B: return format("Basic: ",bv);
        case C: return format("Closure(%s)",ccp);
        case F: return format("Function");
        case V: return format("Vector(%d,%s) ",vn,vv);
        case LNil: return format("Nil");
        case LCons: return format("%s : %s", lhd, ltl);
      }
    }
}

// instructions as functions -- hopefully compiler can inline some

void mkbasic(VM vm) {
  assert(vm.stack[$-1].type == Value.Int);
  vm.stack[$-1] = Value.VAddr(new HObject(vm.stack[$-1].intData));
}

void getbasic(VM vm)
{
  assert(vm.stack[$-1].type          == Value.Address);
  assert(vm.stack[$-1].addrData.type == HObject.B);
  vm.stack[$-1] = Value.VInt(vm.stack[$-1].addrData.bv);
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
  }
}

void add(VM vm) { 
  assert(vm.stack[$-1].type == Value.Int);
  assert(vm.stack[$-1-1].type == Value.Int);
  vm.stack[$-1-1] = Value.VInt(vm.stack[$-1-1].intData + vm.stack[$-1].intData);
  vm.stack = vm.stack[0..$-1];
}


void sub(VM vm) { 
  assert(vm.stack[$-1].type == Value.Int);
  assert(vm.stack[$-1-1].type == Value.Int);
  vm.stack[$-1-1] = Value.VInt(vm.stack[$-1-1].intData - vm.stack[$-1].intData);
  vm.stack = vm.stack[0..$-1];
}

void mul(VM vm) { 
  assert(vm.stack[$-1].type == Value.Int);
  assert(vm.stack[$-1-1].type == Value.Int);
  vm.stack[$-1-1] = Value.VInt(vm.stack[$-1-1].intData * vm.stack[$-1].intData);
  vm.stack = vm.stack[0..$-1];
}

void div(VM vm) { 
  assert(vm.stack[$-1].type == Value.Int);
  assert(vm.stack[$-1-1].type == Value.Int);
  vm.stack[$-1-1] = Value.VInt(vm.stack[$-1-1].intData / vm.stack[$-1].intData);
  vm.stack = vm.stack[0..$-1];
}

void leq(VM vm) { 
  assert(vm.stack[$-1].type == Value.Int);
  assert(vm.stack[$-1-1].type == Value.Int);
  vm.stack[$-1-1] = Value.VInt(vm.stack[$-1-1].intData <= vm.stack[$-1].intData);
  vm.stack = vm.stack[0..$-1];
}

void eq(VM vm) { 
  assert(vm.stack[$-1].type == Value.Int);
  assert(vm.stack[$-1-1].type == Value.Int);
  vm.stack[$-1-1] = Value.VInt(vm.stack[$-1-1].intData == vm.stack[$-1].intData);
  vm.stack = vm.stack[0..$-1];
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
}

void halt(VM vm){ 
  vm.halted = true; 
}

void neg(VM vm) { 
  assert(vm.stack[$-1].type == Value.Int);
  vm.stack[$-1-1] = Value.VInt(- vm.stack[$-1].intData);
}

void not(VM vm) { 
  assert(vm.stack[$-1].type == Value.Int);
  vm.stack[$-1-1] = Value.VInt(! vm.stack[$-1].intData);
}

void mod(VM vm) { 
  assert(vm.stack[$-1].type == Value.Int);
  assert(vm.stack[$-1-1].type == Value.Int);
  vm.stack[$-1-1] = Value.VInt(vm.stack[$-1-1].intData % vm.stack[$-1].intData);
  vm.stack = vm.stack[0..$-1];
}

void neq(VM vm) { 
  assert(vm.stack[$-1].type == Value.Int);
  assert(vm.stack[$-1-1].type == Value.Int);
  vm.stack[$-1-1] = Value.VInt(vm.stack[$-1-1].intData != vm.stack[$-1].intData);
  vm.stack = vm.stack[0..$-1];
}

void le(VM vm) { 
  assert(vm.stack[$-1].type == Value.Int);
  assert(vm.stack[$-1-1].type == Value.Int);
  vm.stack[$-1-1] = Value.VInt(vm.stack[$-1-1].intData < vm.stack[$-1].intData);
  vm.stack = vm.stack[0..$-1];
}

void geq(VM vm) { 
  assert(vm.stack[$-1].type == Value.Int);
  assert(vm.stack[$-1-1].type == Value.Int);
  vm.stack[$-1-1] = Value.VInt(vm.stack[$-1-1].intData >= vm.stack[$-1].intData);
  vm.stack = vm.stack[0..$-1];
}

void gr(VM vm) { 
  assert(vm.stack[$-1].type == Value.Int);
  assert(vm.stack[$-1-1].type == Value.Int);
  vm.stack[$-1-1] = Value.VInt(vm.stack[$-1-1].intData > vm.stack[$-1].intData);
  vm.stack = vm.stack[0..$-1];
}

void or(VM vm) { 
  assert(vm.stack[$-1].type == Value.Int);
  assert(vm.stack[$-1-1].type == Value.Int);
  vm.stack[$-1-1] = Value.VInt(vm.stack[$-1-1].intData || vm.stack[$-1].intData);
  vm.stack = vm.stack[0..$-1];
}

void and(VM vm) { 
  assert(vm.stack[$-1].type == Value.Int);
  assert(vm.stack[$-1-1].type == Value.Int);
  vm.stack[$-1-1] = Value.VInt(vm.stack[$-1-1].intData && vm.stack[$-1].intData);
  vm.stack = vm.stack[0..$-1];
}

void nil(VM vm) { 
  vm.stack ~= Value.VAddr(new HObject());
}

void cons(VM vm) { 
  assert(vm.stack[$-1].type == Value.Address);
  assert(vm.stack[$-2].type == Value.Address);
  vm.stack[$-2] = Value.VAddr(new HObject(vm.stack[$-2].addrData,vm.stack[$-1].addrData));
  vm.stack = vm.stack[0..$-1];
}



void loadc(int arg, VM vm) { 
  vm.stack ~= Value.VInt(arg);
}

void pushloc(int arg, VM vm) { 
  assert(vm.stack[$-1-arg].type == Value.Address);
  vm.stack ~= Value.VAddr(vm.stack[$-1-arg].addrData);
}

void pushglob(int arg, VM vm) { 
  assert(vm.gp.type == HObject.V);
  vm.stack ~= Value.VAddr(vm.gp.vv[arg]);
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
}

void slide(int arg, VM vm) { 
  vm.stack[$-1-arg] = vm.stack[$-1];
  vm.stack = vm.stack[0..$-arg];
}

void alloc(int arg, VM vm) { 
  for(int i = 0; i < arg; ++i){
    vm.stack ~= Value.VAddr(new HObject(-1,cast(HObject)null));
  }
}

void rewrite(int arg, VM vm) { 
  assert(vm.stack[$-1-arg].type == Value.Address);
  assert(vm.stack[$-1].type == Value.Address);
  vm.stack[$-1-arg].addrData.rewrite(vm.stack[$-1].addrData);
  vm.stack = vm.stack[0..$-1];
}

void mkvec(int arg, VM vm) {
  HObject[] arr;
  foreach(e;vm.stack[$-arg..$]){
    assert(e.type == Value.Address);
    arr ~= e.addrData;
  }
  vm.stack = vm.stack[0..$-arg];
  vm.stack ~= Value.VAddr(new HObject(arg,arr));
}

void getvec(int arg, VM vm) { 
  assert(vm.stack[$-1].type == Value.Address);
  assert(vm.stack[$-1].addrData.type == HObject.V );
  HObject o = vm.stack[$-1].addrData;
  vm.stack = vm.stack[0..$-1];
  foreach(e;o.vv)
    vm.stack ~= Value.VAddr(e);
}

void get(int arg, VM vm) { 
  assert(vm.stack[$-1].type == Value.Address);
  assert(vm.stack[$-1].addrData.type == HObject.V);
  vm.stack[$-1] = Value.VAddr(vm.stack[$-1].addrData.vv[arg]);
}


void mkclos(int arg, VM vm) { 
  assert(vm.stack[$-1].type == Value.Address);
  assert(vm.stack[$-1].addrData.type == HObject.V);
  vm.stack[$-1] = Value.VAddr(new HObject(arg,vm.stack[$-1].addrData));
}

void mkfunval(int arg, VM vm) { 
  assert(vm.stack[$-1].type == Value.Address);
  assert(vm.stack[$-1].addrData.type == HObject.V);
  HObject h = new HObject(0,[]);
  vm.stack[$-1] = Value.VAddr(new HObject(arg,h,vm.stack[$-1].addrData));
}

void mark(int arg, VM vm) { 
  vm.stack ~= Value.VAddr(vm.gp);
  vm.stack ~= Value.VInt(vm.fp);
  vm.stack ~= Value.VInt(arg);
  vm.fp = vm.stack.length-1;
}

void jumpz(int arg, VM vm) { 
  assert(vm.stack[$-1].type == Value.Int);
  if (vm.stack[$-1].intData == 0)
    vm.pc = arg;
  vm.stack = vm.stack[0..$-1];
}

void jump(int arg, VM vm) { 
    vm.pc = arg;
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
  } else assert(false,"Not a list");
}

void move(int r, int k, VM vm) { 
  foreach(i,e;vm.stack[$-k..$]){
    vm.stack[$-k-r+i] = e;
  }
  vm.stack = vm.stack[0..$-r];
}




/* string processing  */
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

/*













interface Instruction {
  void exec(VM_ VM_);
}

abstract class aIntInstruction : Instruction {
  public:
    int arg;
  
    this(int i){
      arg = i;
    }
}

abstract class aStringInstruction : Instruction {
  public:
    string arg;
  
    this(string i){
      arg = i;
    }    
}

class MkBasic     
  : Instruction 
{
  void exec(VM_ VM_)
  {
    assert(VM_.stack[$-1].type == Value.Int);
    VM_.stack[$-1] = Value.VAddr(new HObject(VM_.stack[$-1].intData));
  }
}

class GetBasic     
  : Instruction 
{
  void exec(VM_ VM_)
  {
    assert(VM_.stack[$-1].type == Value.Address);
    assert(VM_.stack[$-1].addrData.type == HObject.B);
    VM_.stack[$-1] = Value.VInt(VM_.stack[$-1].addrData.bv);
  }
}

class Eval     
  : Instruction 
{
  void exec(VM_ VM_)
  { 
    assert(VM_.stack[$-1].type == Value.Address);
    if(VM_.stack[$-1].addrData.type == HObject.C){
      // mark pc
      VM_.stack ~= Value.VAddr(VM_.gp);
      VM_.stack ~= Value.VInt(VM_.fp);
      VM_.stack ~= Value.VInt(VM_.pc);
      VM_.fp = VM_.stack.length-1;
      // pushlock 3
      assert(VM_.stack[$-1-3].type == Value.Address);
      VM_.stack ~= Value.VAddr(VM_.stack[$-1-3].addrData);
      // apply0
      assert(VM_.stack[$-1].type == Value.Address);
      assert(VM_.stack[$-1].addrData.type == HObject.C);
      VM_.gp = VM_.stack[$-1].addrData.cgp;
      VM_.pc = VM_.stack[$-1].addrData.ccp;
      VM_.stack = VM_.stack[0..$-1];
    }
  }
}

class Add      
  : Instruction 
{
  void exec(VM_ VM_)
  { 
    assert(VM_.stack[$-1].type == Value.Int);
    assert(VM_.stack[$-1-1].type == Value.Int);
    VM_.stack[$-1-1] = Value.VInt(VM_.stack[$-1-1].intData + VM_.stack[$-1].intData);
    VM_.stack = VM_.stack[0..$-1];
  }
}
class Sub      
  : Instruction 
{
  void exec(VM_ VM_)
  { 
    assert(VM_.stack[$-1].type == Value.Int);
    assert(VM_.stack[$-1-1].type == Value.Int);
    VM_.stack[$-1-1] = Value.VInt(VM_.stack[$-1-1].intData - VM_.stack[$-1].intData);
    VM_.stack = VM_.stack[0..$-1];
  }
}

class Mul      
  : Instruction 
{
  void exec(VM_ VM_)
  { 
    assert(VM_.stack[$-1].type == Value.Int);
    assert(VM_.stack[$-1-1].type == Value.Int);
    VM_.stack[$-1-1] = Value.VInt(VM_.stack[$-1-1].intData * VM_.stack[$-1].intData);
    VM_.stack = VM_.stack[0..$-1];
  }
}

class Div      
  : Instruction 
{
  void exec(VM_ VM_)
  { 
    assert(VM_.stack[$-1].type == Value.Int);
    assert(VM_.stack[$-1-1].type == Value.Int);
    VM_.stack[$-1-1] = Value.VInt(VM_.stack[$-1-1].intData / VM_.stack[$-1].intData);
    VM_.stack = VM_.stack[0..$-1];
  }
}

class Leq      
  : Instruction 
{
  void exec(VM_ VM_)
  { 
    assert(VM_.stack[$-1].type == Value.Int);
    assert(VM_.stack[$-1-1].type == Value.Int);
    VM_.stack[$-1-1] = Value.VInt(VM_.stack[$-1-1].intData <= VM_.stack[$-1].intData);
    VM_.stack = VM_.stack[0..$-1];
  }
}

class Eq       
  : Instruction 
{
  void exec(VM_ VM_)
  { 
    assert(VM_.stack[$-1].type == Value.Int);
    assert(VM_.stack[$-1-1].type == Value.Int);
    VM_.stack[$-1-1] = Value.VInt(VM_.stack[$-1-1].intData == VM_.stack[$-1].intData);
    VM_.stack = VM_.stack[0..$-1];
  }
}

class Apply      
  : Instruction 
{
  void exec(VM_ VM_)
  {
    assert(VM_.stack[$-1].type == Value.Address);
    assert(VM_.stack[$-1].addrData.type == HObject.F);
    assert(VM_.stack[$-1].addrData.fap.type == HObject.V);
    HObject h = VM_.stack[$-1].addrData;
    VM_.stack = VM_.stack[0..$-1];
    VM_.gp = h.fgp;
    VM_.pc = h.fcp;
    foreach(e;h.fap.vv){
      VM_.stack ~= Value.VAddr(e);
    }
  }
}

class Update     
  : Instruction 
{
  void exec(VM_ VM_)
  { 
      auto fp = VM_.fp ;
      
      assert(VM_.stack[fp].type == Value.Int);
      VM_.pc = VM_.stack[fp].intData;

      assert(VM_.stack[fp-2].type == Value.Address);
      VM_.gp = VM_.stack[fp-2].addrData;

      assert(VM_.stack[fp-1].type == Value.Int);
      VM_.fp = VM_.stack[fp-1].intData;

      assert(VM_.stack[$-1].type == Value.Address);
      assert(VM_.stack[fp-3].type == Value.Address);
      VM_.stack[fp-3].addrData.rewrite(VM_.stack[$-1].addrData);

      VM_.stack = VM_.stack[0..fp-2];
  }
}

class Halt     
  : Instruction 
{
  void exec(VM_ VM_){ VM_.halted = true; }
}

class Neg     
  : Instruction 
{
  void exec(VM_ VM_)
  { 
    assert(VM_.stack[$-1].type == Value.Int);
    VM_.stack[$-1-1] = Value.VInt(- VM_.stack[$-1].intData);
  }
}

class Not     
  : Instruction 
{
  void exec(VM_ VM_)
  { 
    assert(VM_.stack[$-1].type == Value.Int);
    VM_.stack[$-1-1] = Value.VInt(! VM_.stack[$-1].intData);
  }
}

class Mod      
  : Instruction 
{
  void exec(VM_ VM_)
  { 
    assert(VM_.stack[$-1].type == Value.Int);
    assert(VM_.stack[$-1-1].type == Value.Int);
    VM_.stack[$-1-1] = Value.VInt(VM_.stack[$-1-1].intData % VM_.stack[$-1].intData);
    VM_.stack = VM_.stack[0..$-1];
  }
}

class Neq      
  : Instruction 
{
  void exec(VM_ VM_)
  { 
    assert(VM_.stack[$-1].type == Value.Int);
    assert(VM_.stack[$-1-1].type == Value.Int);
    VM_.stack[$-1-1] = Value.VInt(VM_.stack[$-1-1].intData != VM_.stack[$-1].intData);
    VM_.stack = VM_.stack[0..$-1];
  }
}

class Le      
  : Instruction 
{
  void exec(VM_ VM_)
  { 
    assert(VM_.stack[$-1].type == Value.Int);
    assert(VM_.stack[$-1-1].type == Value.Int);
    VM_.stack[$-1-1] = Value.VInt(VM_.stack[$-1-1].intData < VM_.stack[$-1].intData);
    VM_.stack = VM_.stack[0..$-1];
  }
}

class Geq     
  : Instruction 
{
  void exec(VM_ VM_)
  { 
    assert(VM_.stack[$-1].type == Value.Int);
    assert(VM_.stack[$-1-1].type == Value.Int);
    VM_.stack[$-1-1] = Value.VInt(VM_.stack[$-1-1].intData >= VM_.stack[$-1].intData);
    VM_.stack = VM_.stack[0..$-1];
  }
}

class Gr      
  : Instruction 
{
  void exec(VM_ VM_)
  { 
    assert(VM_.stack[$-1].type == Value.Int);
    assert(VM_.stack[$-1-1].type == Value.Int);
    VM_.stack[$-1-1] = Value.VInt(VM_.stack[$-1-1].intData > VM_.stack[$-1].intData);
    VM_.stack = VM_.stack[0..$-1];
  }
}

class Or     
  : Instruction 
{
  void exec(VM_ VM_)
  { 
    assert(VM_.stack[$-1].type == Value.Int);
    assert(VM_.stack[$-1-1].type == Value.Int);
    VM_.stack[$-1-1] = Value.VInt(VM_.stack[$-1-1].intData || VM_.stack[$-1].intData);
    VM_.stack = VM_.stack[0..$-1];
  }
}

class And      
  : Instruction 
{
  void exec(VM_ VM_)
  { 
    assert(VM_.stack[$-1].type == Value.Int);
    assert(VM_.stack[$-1-1].type == Value.Int);
    VM_.stack[$-1-1] = Value.VInt(VM_.stack[$-1-1].intData && VM_.stack[$-1].intData);
    VM_.stack = VM_.stack[0..$-1];
  }
}

class Nil      
  : Instruction 
{
  void exec(VM_ VM_)
  { 
    VM_.stack ~= Value.VAddr(new HObject());
  }
}

class Cons     
  : Instruction 
{
  void exec(VM_ VM_)
  { 
    assert(VM_.stack[$-1].type == Value.Address);
    assert(VM_.stack[$-2].type == Value.Address);
    VM_.stack[$-2] = Value.VAddr(new HObject(VM_.stack[$-2].addrData,VM_.stack[$-1].addrData));
    VM_.stack = VM_.stack[0..$-1];
  }
}


class Loadc    
  : aIntInstruction 
{ 
  this(int i) { super(i); } 
  void exec(VM_ VM_)
  { 
    VM_.stack ~= Value.VInt(arg);
  }
}

class PushLoc   
  : aIntInstruction 
{ 
  this(int i) { super(i); } 
  void exec(VM_ VM_)
  { 
    assert(VM_.stack[$-1-arg].type == Value.Address);
    VM_.stack ~= Value.VAddr(VM_.stack[$-1-arg].addrData);
  }
}

class PushGlob 
  : aIntInstruction 
{ 
  this(int i) { super(i); } 
  void exec(VM_ VM_)
  { 
    assert(VM_.gp.type == HObject.V);
    VM_.stack ~= Value.VAddr(VM_.gp.vv[arg]);
  }
}

class TArg
  : aIntInstruction 
{ 
  this(int i) { super(i); } 
  void exec(VM_ VM_)
  { 
    int g = VM_.stack.length-1-VM_.fp;
    auto fp = VM_.fp;
    if (g < arg){
      // mkvec0
      HObject[] arr = new HObject[g]; 
      foreach(i,e;VM_.stack[$-g..$].reverse){
        assert(e.type == Value.Address);
        arr[i] = e.addrData;
      }
      auto ap = new HObject(g,arr);

      //  wrap A
      HObject f = new HObject(VM_.pc-1,ap,VM_.gp);

      // popenv      

      assert(VM_.stack[fp-2].type == Value.Address);
      VM_.gp = VM_.stack[fp-2].addrData;
      VM_.stack[fp-2] = Value.VAddr(f); 

      assert(VM_.stack[fp].type == Value.Int);
      VM_.pc = VM_.stack[fp].intData;

      assert(VM_.stack[fp-1].type == Value.Int);
      VM_.fp = VM_.stack[fp-1].intData;

      VM_.stack = VM_.stack[0..fp-1];
    }
  }
}

class Return  
  : aIntInstruction 
{ 
  this(int i) { super(i); } 
  void exec(VM_ VM_)
  { 
    auto sp = VM_.stack.length-1;
    auto fp = VM_.fp;
    if (sp - fp == arg + 1){
      // popenv
      assert(VM_.stack[fp-2].type == Value.Address);
      VM_.gp = VM_.stack[fp-2].addrData;      
      
      VM_.stack[fp-2] = VM_.stack[$-1] ;
      
      assert(VM_.stack[fp].type == Value.Int);
      VM_.pc = VM_.stack[fp].intData;
            
      assert(VM_.stack[fp-1].type == Value.Int);
      VM_.fp = VM_.stack[fp-1].intData;
      
      VM_.stack = VM_.stack[0..fp-1];
    } else {
      // slide k
      VM_.stack[$-1-arg] = VM_.stack[$-1];
      VM_.stack = VM_.stack[0..$-arg];
      // apply
      assert(VM_.stack[$-1].type == Value.Address);
      assert(VM_.stack[$-1].addrData.type == HObject.F);
      assert(VM_.stack[$-1].addrData.fap.type == HObject.V);
      HObject h = VM_.stack[$-1].addrData;
      VM_.stack = VM_.stack[0..$-1];
      VM_.gp = h.fgp;
      VM_.pc = h.fcp;
      foreach(e;h.fap.vv){
        VM_.stack ~= Value.VAddr(e);
      }
    }
  }
}

class Slide 
  : aIntInstruction 
{ 
  this(int i) { super(i); } 
  void exec(VM_ VM_)
  { 
    VM_.stack[$-1-arg] = VM_.stack[$-1];
    VM_.stack = VM_.stack[0..$-arg];
  }
}

class Alloc
  : aIntInstruction 
{ 
  this(int i) { super(i); } 
  void exec(VM_ VM_)
  { 
    for(int i = 0; i < arg; ++i){
      VM_.stack ~= Value.VAddr(new HObject(-1,cast(HObject)null));
    }
  }
}

class Rewrite
  : aIntInstruction 
{ 
  this(int i) { super(i); } 
  void exec(VM_ VM_)
  { 
    assert(VM_.stack[$-1-arg].type == Value.Address);
    assert(VM_.stack[$-1].type == Value.Address);
    VM_.stack[$-1-arg].addrData.rewrite(VM_.stack[$-1].addrData);
    VM_.stack = VM_.stack[0..$-1];
  }
}

class MkVec
  : aIntInstruction 
{ 
  this(int i) { super(i); } 
  void exec(VM_ VM_)
  {
    HObject[] arr;
    foreach(e;VM_.stack[$-arg..$]){
      assert(e.type == Value.Address);
      arr ~= e.addrData;
    }
    VM_.stack = VM_.stack[0..$-arg];
    VM_.stack ~= Value.VAddr(new HObject(arg,arr));
  }
}

class GetVec 
  : aIntInstruction 
{ 
  this(int i) { super(i); } 
  void exec(VM_ VM_)
  { 
    assert(VM_.stack[$-1].type == Value.Address);
    assert(VM_.stack[$-1].addrData.type == HObject.V );
    HObject o = VM_.stack[$-1].addrData;
    VM_.stack = VM_.stack[0..$-1];
    foreach(e;o.vv)
      VM_.stack ~= Value.VAddr(e);
  }
}


class Get   
  : aIntInstruction 
{ 
  this(int i) { super(i); } 
  void exec(VM_ VM_)
  { 
  }
}





class MkClos   
  : aStringInstruction 
{  
  this(string i) 
  { 
    super(i); 
  }
  void exec(VM_ VM_)
  { 
    assert(VM_.stack[$-1].type == Value.Address);
    assert(VM_.stack[$-1].addrData.type == HObject.V);
    VM_.stack[$-1] = Value.VAddr(new HObject(VM_.lables[arg],VM_.stack[$-1].addrData));
  }
}

class MkFunVal 
  : aStringInstruction 
{  
  this(string i) 
  { 
    super(i); 
  }
  void exec(VM_ VM_)
  { 
    assert(VM_.stack[$-1].type == Value.Address);
    assert(VM_.stack[$-1].addrData.type == HObject.V);
    HObject h = new HObject(0,[]);
    VM_.stack[$-1] = Value.VAddr(new HObject(VM_.lables[arg],h,VM_.stack[$-1].addrData));
  }
}

class Mark 
  : aStringInstruction 
{  
  this(string i) 
  { 
    super(i); 
  }
  void exec(VM_ VM_)
  { 
    VM_.stack ~= Value.VAddr(VM_.gp);
    VM_.stack ~= Value.VInt(VM_.fp);
    VM_.stack ~= Value.VInt(VM_.lables[arg]);
    VM_.fp = VM_.stack.length-1;
  }
}

class Jumpz    
  : aStringInstruction 
{  
  this(string i) 
  { 
    super(i); 
  }
  void exec(VM_ VM_)
  { 
    assert(VM_.stack[$-1].type == Value.Int);
    if (VM_.stack[$-1].intData == 0)
      VM_.pc = VM_.lables[arg];
    VM_.stack = VM_.stack[0..$-1];
  }
}

class Jump 
  : aStringInstruction 
{  
  this(string i) 
  { 
    super(i); 
  }
  void exec(VM_ VM_)
  { 
      VM_.pc = VM_.lables[arg];
  }
}

class TList
  : aStringInstruction 
{  
  this(string i) 
  { 
    super(i); 
  }
  void exec(VM_ VM_)
  { 
    assert(VM_.stack[$-1].type == Value.Address);
    
    if (VM_.stack[$-1].addrData.type == HObject.LNil) {
      VM_.stack = VM_.stack[0..$-1];
    } else if (VM_.stack[$-1].addrData.type == HObject.LCons) {
      auto list = VM_.stack[$-1].addrData;
      VM_.stack[$-1]  = Value.VAddr(list.lhd);
      VM_.stack      ~= Value.VAddr(list.ltl);
      VM_.pc = VM_.lables[arg];
    } else assert(false,"Not a list");
  }
}

class Move
  : Instruction
{  
  int r, k;
  
  this(int r, int k) 
  { 
    this.r = r;
    this.k = k;
  }
  void exec(VM_ VM_)
  { 
    foreach(i,e;VM_.stack[$-k..$]){
      VM_.stack[$-k-r+i] = e;
    }
    VM_.stack = VM_.stack[0..$-r];
  }
}


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

struct Value {
  enum : int {Int, Address};
  int type;
  union{
    int     intData;
    HObject addrData;
  }
  
  static Value VInt(int i){
    Value v;
    v.type    = Int;
    v.intData = i;
    return v;
  }

  static Value VAddr(HObject i){
    Value v;
    v.type     = Address;
    v.addrData = i;
    return v;
  }
  
  string toString(){    
    return type==Int ? format(intData) : format("@",addrData);
  }
  
}

class HObject {
  public:
    enum : int {B, C, F, V, LNil,LCons};
    int type;
    union{
      int bv;
      struct{
        int     ccp;
        HObject cgp;
      }
      struct{
        int     fcp;
        HObject fap,fgp;
      }
      struct{
        int       vn;
        HObject[] vv;
      }
      struct{
        HObject   lhd;
        HObject   ltl;
      }
    }
    
    this(){
      type = LNil;
    }

    this(int i){
      type = B;
      bv = i;
    }

    this(int i, HObject o){
      type = C;
      ccp  = i;
      cgp  = o;
    }

    this(HObject i, HObject o){
      type = LCons;
      lhd  = i;
      ltl  = o;
    }

    this(int i, HObject o, HObject s){
      type = F;
      fcp  = i;
      fap  = o;
      fgp  = s;
    }

    this(int i, HObject[] o){
      type = V;
      vn   = i;
      vv   = o;
    }
    
    void rewrite(HObject o){
      switch(type = o.type){
        case B: bv = o.bv; break;
        case C: ccp = o.ccp; cgp = o.cgp; break;
        case F: fcp = o.fcp; fap = o.fap; fgp = o.fgp; break;
        case V: vn  = o.vn;  vv  = o.vv ; break;
        case LNil: break;
        case LCons: lhd = o.lhd; ltl = o.ltl; break;
      }
    }
    
    string toString(){
      switch (type) {
        case B: return format("Basic: ",bv);
        case C: return format("Closure(%s)",ccp);
        case F: return format("Function");
        case V: return format("Vector(%d,%s) ",vn,vv);
        case LNil: return format("Nil");
        case LCons: return format("%s : %s", lhd, ltl);
      }
    }
}

class VM_ {
  protected:
    int[string]   lables;
    Instruction[] instr;
    int           pc;
    
    Value[]       stack;
    int           fp;
    HObject       gp;
    
    bool          halted;
  public:
    this(){
      pc = 0;
      fp = -1;
      halted = false;
    }
  
    void step(){
      if (! halted){
        instr[pc++].exec(this);
      }
    }

    void run(){
      while (! halted){
        instr[pc++].exec(this);
      }
    }
    
    void readMama(string str)
    {
      Instruction[] res;
      string[]      words;
      
      words = str.filterComments().tolower().split();
      
      while (words.length) {
        switch (words[0]) {
          case "mkbasic"  : res ~= new MkBasic                  ; words = words[1..$]; break;
          case "getbasic" : res ~= new GetBasic                 ; words = words[1..$]; break;
          case "eval"     : res ~= new Eval                     ; words = words[1..$]; break;
          case "add"      : res ~= new Add                      ; words = words[1..$]; break;
          case "sub"      : res ~= new Sub                      ; words = words[1..$]; break;
          case "mul"      : res ~= new Mul                      ; words = words[1..$]; break;
          case "div"      : res ~= new Div                      ; words = words[1..$]; break;
          case "leq"      : res ~= new Leq                      ; words = words[1..$]; break;
          case "eq"       : res ~= new Eq                       ; words = words[1..$]; break;
          case "apply"    : res ~= new Apply                    ; words = words[1..$]; break;
          case "update"   : res ~= new Update                   ; words = words[1..$]; break;
          case "halt"     : res ~= new Halt                     ; words = words[1..$]; break;
          case "neg"      : res ~= new Neg                      ; words = words[1..$]; break;
          case "not"      : res ~= new Not                      ; words = words[1..$]; break;
          case "mod"      : res ~= new Mod                      ; words = words[1..$]; break;
          case "neq"      : res ~= new Neq                      ; words = words[1..$]; break;
          case "le"       : res ~= new Le                       ; words = words[1..$]; break;
          case "geq"      : res ~= new Geq                      ; words = words[1..$]; break;
          case "gr"       : res ~= new Gr                       ; words = words[1..$]; break;
          case "or"       : res ~= new Or                       ; words = words[1..$]; break;
          case "and"      : res ~= new And                      ; words = words[1..$]; break;
          case "nil"      : res ~= new Nil                      ; words = words[1..$]; break;
          case "cons"     : res ~= new Cons                     ; words = words[1..$]; break;
          case "loadc"    : res ~= new Loadc   (atoi(words[1])) ; words = words[2..$]; break;
          case "pushloc"  : res ~= new PushLoc (atoi(words[1])) ; words = words[2..$]; break;
          case "pushglob" : res ~= new PushGlob(atoi(words[1])) ; words = words[2..$]; break;
          case "targ"     : res ~= new TArg    (atoi(words[1])) ; words = words[2..$]; break;
          case "return"   : res ~= new Return  (atoi(words[1])) ; words = words[2..$]; break; 
          case "slide"    : res ~= new Slide   (atoi(words[1])) ; words = words[2..$]; break;
          case "alloc"    : res ~= new Alloc   (atoi(words[1])) ; words = words[2..$]; break;
          case "rewrite"  : res ~= new Rewrite (atoi(words[1])) ; words = words[2..$]; break;
          case "mkvec"    : res ~= new MkVec   (atoi(words[1])) ; words = words[2..$]; break;
          case "getvec"   : res ~= new GetVec  (atoi(words[1])) ; words = words[2..$]; break;
          case "get"      : res ~= new Get     (atoi(words[1])) ; words = words[2..$]; break;
          case "mkclos"   : res ~= new MkClos  (words[1])       ; words = words[2..$]; break;
          case "mkfunval" : res ~= new MkFunVal(words[1])       ; words = words[2..$]; break;
          case "mark"     : res ~= new Mark    (words[1])       ; words = words[2..$]; break;
          case "jumpz"    : res ~= new Jumpz   (words[1])       ; words = words[2..$]; break;
          case "jump"     : res ~= new Jump    (words[1])       ; words = words[2..$]; break;
          case "tlist"    : res ~= new TList   (words[1])       ; words = words[2..$]; break;
          case "move"     : res ~= new Move(atoi(words[1]),atoi(words[2]))  ; words = words[3..$]; break;
          default:
            if (words[0][$-1] == ':'){
              assert(!(words[0][0..$-1] in lables));
              lables[words[0][0..$-1]] = res.length;
              words = words[1..$];
            } else if (words.length && words[1] == ":"){
              assert(!(words[0] in lables));
              lables[words[0]] = res.length;
              words = words[2..$];
            } else {
              writefln("parse error on: %s",words[0]);
              return;
            }
        }
      }
      
      instr = res;
    }


    string toString(){
      string str = format("PC = %d \t SP = %d \t FP = %d\n GP = %s \n STACK: \n",pc,stack.length-1,fp,gp);
      
      foreach(i,e;stack){
        str ~= format("%s\n",e);
        }
      
      return str;
    }
}

*/


int main(string[] args)
{
  if (args.length != 2){
    writefln("mama <file.cbn>");
    return 0;
  }
  
  VM v1 = new VM;
  v1.readMama(cast(char[])read(args[1]));
  
  int step = 0;
  while (!v1.halted){
    step++;
//    writefln("%d:  instr: %s",step, show_instr(v1.ins[v1.pc]));
    v1.step();
//    writefln(v1);
  }
  writefln("program halted after %d steps", step);
  writefln(v1);

  return 0;
  
}
