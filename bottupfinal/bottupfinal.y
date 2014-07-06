%{
package bottupfinal

import (
	"io"
	"strconv"
	"fmt"
	"os"
	"unicode"
	"bytes"
	"strings"
)

var ast = NewTree(Tprograma, nil)
%}

%union{
	num float64
	name string
	sym *Sym
	syms []*Sym
	tree *Tree
	trees []*Tree
}

%type <num> LIT
%type <name> VAR TIPO EXPR
%type <tree> MACRO DECL SENT LOOP
%type <trees> DECLS SENTS
%type <syms> PARAMS PARAM NEXT_PARAM STMT FUNC ASIG IN_FUNS
%type <sym> IN_FUN FIRST_IN

%token <num> litint litfloat
%token <name> nombre tipo
%token macro loop 
%token '{' '}' '(' ')' ',' ';' ':' '='

%left '+'  '-'
%left '*'  '/'   

%%
MACROS : MACROS MACRO
	{ ast.Ramas = append(ast.Ramas, $2) }
	| MACRO
	{ ast.Ramas = append(ast.Ramas, $1) }
	;

MACRO : macro VAR '(' PARAMS
	{
		simboloMacro := createSym($2, Smacro, nombre, 1)
	  	tabla = pushSim(tabla, $2, simboloMacro)
		tabla = pushTabla(tabla, make(Ambito))
	}
	')' '{' DECLS SENTS '}'
	{ 
		tabla = popTabla(tabla)
		simboloMacro := createSym($2, Smacro, nombre, 1)
		syms := []*Sym{simboloMacro}
		for i := range $4 {
			syms = append(syms, $4[i])
		}
		macroTree := NewTree(Tmacro, syms) 
		for i := range $8 {
			macroTree.Ramas = append(macroTree.Ramas, $8[i])
		}
		for i := range $9 {
			macroTree.Ramas = append(macroTree.Ramas, $9[i])
		}
		$$ = macroTree
	}
	;

LIT : litint
	| litfloat
	;
	
VAR : nombre
	;

EXPR : 	EXPR '+' EXPR
	{ 
		var1, err1 := strconv.ParseFloat($1, 64)
		var2, err2 := strconv.ParseFloat($3, 64)
		if (err1!=nil) || (err2!=nil) {
			$$ = fmt.Sprintf("%v+%v", $1, $3) 
		}
		$$ = fmt.Sprintf("%v", var1 + var2) 
	}
	| EXPR '-' EXPR
	{ 
		var1, err1 := strconv.ParseFloat($1, 64)
		var2, err2 := strconv.ParseFloat($3, 64)
		if (err1!=nil) || (err2!=nil) {
			$$ = fmt.Sprintf("%v-%v", $1, $3) 
		}
		$$ = fmt.Sprintf("%v", var1 - var2) 	
	}
	| EXPR '*' EXPR
	{ 
		var1, err1 := strconv.ParseFloat($1, 64)
		var2, err2 := strconv.ParseFloat($3, 64)
		if (err1!=nil) || (err2!=nil) {
			$$ = fmt.Sprintf("%v*%v", $1, $3) 
		}
		$$ = fmt.Sprintf("%v", var1 * var2) 
	}
	| EXPR '/' EXPR
	{ 
		var1, err1 := strconv.ParseFloat($1, 64)
		var2, err2 := strconv.ParseFloat($3, 64)
		if (err1!=nil) || (err2!=nil) {
			$$ = fmt.Sprintf("%v*%v", $1, $3) 
		}
		$$ = fmt.Sprintf("%v", var1 / var2)  
	}
	| VAR
	{ 
		if !IsDeclared(tabla, createSym($1, Svar, nombre, 1)) {
			fmt.Println("symbol "+$1+" not declared")
		}
		$$ = $1 
	}
	| LIT
	{ $$ = fmt.Sprintf("%v", $1) }
	;

TIPO : tipo
	;

DECLS : 
	{ $$ = nil }
	| DECLS DECL
	{ 
		var trees []*Tree
		for i := range $1 {
			trees = append(trees, $1[i])
		} 
		$$ = append(trees, $2)
	}
	;

DECL : PARAM ';'
	{ $$ = NewTree(Tdecl, $1) }
	;

PARAM : TIPO VAR
	{
	  simTipo := createSym($1, Skey, nombre, 1)
	  simVar := createSym($2, Svar, nombre, 1)
	  tabla = pushSim(tabla, $2, simVar)
	  var syms []*Sym
	  syms = append(syms, simTipo)
	  $$ = append(syms, simVar)
	}
	;
		
NEXT_PARAM :
	{ $$ = nil }
	| ',' PARAM
	{ $$ = $2 }
	;

PARAMS : 
	{ $$ = nil }
	| PARAMS PARAM NEXT_PARAM
	{  
		var syms []*Sym
		for i := range $1 {
			syms = append(syms, $1[i])
		}
		for i := range $2 {
			syms = append(syms, $2[i])
		}
		for i := range $3 {
			syms = append(syms, $3[i])
		}
		$$ = syms 
	}
	;

SENTS : 
	{ $$ = nil }
	| SENTS SENT
	{ 
		var trees []*Tree
		for i := range $1 {
			trees = append(trees, $1[i])
		} 
		$$ = append(trees, $2)
	}			
	;

SENT : LOOP 		
	| VAR STMT ';'
	{
		var syms []*Sym
		simVar := createSym($1, Svar, nombre, 1)
		syms = append(syms, simVar)
		for i := range $2 {
			syms = append(syms, $2[i])
		}
		if len($2)==1 {
			$$ = NewTree(Tasig, syms)
		} else {
			$$ = NewTree(Tfunc, syms)
		}
	}
	| ';'
	{ $$ = nil }
	;
				
LOOP : loop VAR 
	{
		 tabla = pushTabla(tabla, make(Ambito))
		 simVar := createSym($2, Svar, nombre, 1)
	  	 tabla = pushSim(tabla, $2, simVar)
	} 
	':' EXPR ',' EXPR '{' SENTS '}'
	{	
		tabla = popTabla(tabla)
		var syms []*Sym
		var simMin *Sym
		var simMax *Sym
		simVar := createSym($2, Svar, nombre, 1)
		_, err := strconv.ParseFloat($5, 64)
		if err!=nil {
			simMin = createSym($5, Svar, nombre, 1)
		} else {
			simMin = createSym($5, Slit, nombre, 1)
		}
		_, err = strconv.ParseFloat($7, 64)
		if err!=nil {
			simMax = createSym($7, Svar, nombre, 1)
		} else {
			simMax = createSym($7, Slit, nombre, 1)
		}	
		syms = append(syms, simVar)
		syms = append(syms, simMin)
		syms = append(syms, simMax)
		loopTree := NewTree(Tloop, syms)
		for i := range $9 {
			loopTree.Ramas = append(loopTree.Ramas, $9[i])
		}
		$$ = loopTree
	} 
	;

STMT : FUNC	
	| ASIG	
	;

IN_FUN : ',' EXPR
	{
		_, err := strconv.ParseFloat($2, 64)
		if err!=nil {
			$$ = createSym($2, Svar, nombre, 1)
		} else {
			$$ = createSym($2, Slit, nombre, 1)
		}
	}
	;

FIRST_IN : 
	{ $$ = nil }
	| EXPR
	{
		_, err := strconv.ParseFloat($1, 64)
		if err!=nil {
			$$ = createSym($1, Svar, nombre, 1)
		} else {
			$$ = createSym($1, Slit, nombre, 1)
		}
	}
	;

IN_FUNS : 
	{ $$ = nil }
	| IN_FUNS FIRST_IN IN_FUN
	{
		var syms []*Sym
		for i := range $1 {
			syms = append(syms, $1[i])
		}
		syms = append(syms, $2)
		$$ = append(syms, $3)
	} 	
	;

FUNC : '(' IN_FUNS ')'
	{ 
		var syms []*Sym
		for i := range $2 {
			if $2[i]!= nil {
				syms = append(syms, $2[i])
			}
		}
		$$ = syms
	}
	;

ASIG : '=' EXPR
	{ 
		var syms []*Sym
		var sim *Sym
		_, err := strconv.ParseFloat($2, 64)
		if err!=nil {
			sim = createSym($2, Svar, nombre, 1)
		} else {
			sim = createSym($2, Slit, nombre, 1)
		}
		$$ = append(syms, sim)
	}
	;

%%

//Generación de código
func BuscaMacro(name string, arbol *Tree) int {
	for i := range arbol.Ramas {
		if arbol.Ramas[i].symList[0].name==name {
			return i
		}
	}
	return -1
}

func GetValue(n string, tablav map[string]float64) float64 {
	f, errf := strconv.ParseFloat(n, 64)
	if errf==nil {
		return f
	}
	return tablav[n]
}

func RecorreCodigo(t *Tree, arbol *Tree, values map[string]float64) (string, map[string]float64) {
	if t==nil {
		return "", values
	}
	switch t.kind {
	case Tmacro:
		return "macro inside macro is not allowed", values
	case Tdecl:
		return "", values
	case Tloop:
		v := values
		result := "#loop\n"
		iter := t.symList[0].name
		minv := int64(GetValue(t.symList[1].name, v))
		maxv := int64(GetValue(t.symList[2].name, v))
		
		for i:=minv; i<=maxv; i++ {
			v[iter] = float64(i)
			for k := range t.Ramas {
				s, vals := RecorreCodigo(t.Ramas[k], arbol, v)
				result += s
				v = vals
			}
		}
		delete(v, iter)
		return result, v
	case Tfunc:
		name := t.symList[0].name
		if strings.EqualFold(name, "rect") || strings.EqualFold(name, "circle") {
			result := name
			for i:=1; i<len(t.symList); i++ {
				result += " "+fmt.Sprintf("%v",GetValue(t.symList[i].name, values))
			}
			return result+"\n", values
		}
		var arg []float64
		for i:=1; i<len(t.symList); i++ {
			arg = append(arg, GetValue(t.symList[i].name, values))
		}
		return CodigoMacro(name, arbol, arg, values)
	case Tasig:
		v := values
		lval := t.symList[0].name
		rval := GetValue(t.symList[1].name, values)
		v[lval] = rval
		return "", v
	}
	return "", values
}

func CodigoMacro(macroName string, t *Tree, args []float64, tablav map[string]float64) (string, map[string]float64) {
	result := "#"+macroName+"("
	valores := tablav
	ramas := t.Ramas
	indice := BuscaMacro(macroName, t)


		for i:=1; i<len(t.symList); i++ {
			result += t.symList[i].name
			if i != len(t.symList)-1 {
				result += ","
			}
		}


	if args!=nil {
		for i := range args {
			tablav[ramas[indice].symList[2+2*i].name] = args[i]
			result += fmt.Sprintf("%v", args[i])
			if i != len(args)-1 {
				result += ", "
			}
		}
	}
	result += ")\n"
	s := ""
	for i := range ramas[indice].Ramas {
		s, valores = RecorreCodigo(ramas[indice].Ramas[i], t, valores)
		result += s
	}
	return result, valores
}

func PintaCodigo(t *Tree) string {
	tablaValores := make(map[string]float64)
	s, _ := CodigoMacro("main", t, nil, tablaValores)
	return "Intérprete:\n"+s
}

//AST
type Tkind int
type Tree struct {
	kind Tkind
	symList []*Sym
	Ramas []*Tree
}

const (
	Tnone Tkind=iota
	Tprograma
	Tmacro
	Tdecl
	Tloop
	Tfunc
	Tasig
)

func NewTree(name Tkind, list []*Sym) *Tree {
	return &Tree{kind: name, symList: list}
}

func AddTabs(number int) string {
	result := ""
	for i:=0;i<number;i++ {
		result += "\t"
	}
	return result
}

func RamaString(t *Tree) string {
	if t==nil {
		return ""
	}
	switch t.kind {
	case Tmacro:
		result := "macro "+t.symList[0].name+"("
		for i:=1; i<=(len(t.symList)-1)/2; i++ {
			result += t.symList[i*2-1].name+" "
			result += t.symList[i*2].name
			if i != (len(t.symList)-1)/2 {
				result += ","
			}
		}
		return result+")"
	case Tdecl:
		return t.symList[0].name+" "+t.symList[1].name+";"
	case Tloop:
		return "loop "+t.symList[0].name+":"+t.symList[1].name+","+t.symList[2].name
	case Tfunc:
		result := t.symList[0].name+"("
		for i:=1; i<len(t.symList); i++ {
			result += t.symList[i].name
			if i != len(t.symList)-1 {
				result += ","
			}
		}
		return result+");"
	case Tasig:
		return t.symList[0].name+" = "+t.symList[1].name+";"
	}
	return ""
}

func RecorreArbol(t *Tree, tabs int) string {
	result := ""
	ramas := t.Ramas
	var ramo *Tree
	for i := range ramas {
		result += AddTabs(tabs+1)
		result += RamaString(ramas[i])+"\n"
		for k := range ramas[i].Ramas {
			ramo = ramas[i].Ramas[k]
			result += AddTabs(tabs+2)+RamaString(ramo)+"\n" + RecorreArbol(ramo, tabs+2)
		}
	}
	return result
}

func PintaArbol(t *Tree) string {
	return "AST:\n" + RecorreArbol(t, 0)
}

//Tabla de simbolos
type Skind int
type Ambito map[string]*Sym
type Pila []Ambito
var tabla = InicializaTabla()

const (
	Snone Skind=iota
	Svar
	Smacro
	Skey
	Slit
)

type Sym struct {
	name string
	kind Skind
	tokid int
	line int
}

func pushTabla(p Pila, a Ambito) Pila{
	return append(p, a)
}

func popTabla(p Pila) Pila { 
	return p[:len(p)-1]
}

func pushSim(p Pila, n string, s *Sym) Pila {
	p[len(p)-1][n]=s
	return p
}

func getSym(p Pila, n string) *Sym {
	for i := range p {
		for k, v := range p[i] {
			if k==n {
				return v
			}
		}
	}
	return nil
}

func createSym(n string, k Skind, t int, l int) *Sym {
	return &Sym{name:n, kind: k, tokid: t, line: l}
}

func IsDeclared(p Pila, s *Sym) bool {
	if s.kind==Slit {
		return true
	}
	for i := range p {
		for k, _ := range p[i] {
			if k==s.name {
				return true
			}
		}
	}
	return false
}

func IsDuplicated(p Pila, s *Sym) bool {
	for k, _ := range p[len(p)-1] {
		if k==s.name {
			return true
		}
	}
	return false
} 

func InicializaTabla() Pila {
	var p Pila
	simboloInt := createSym("int", Skey, tipo, 1)
	simboloFloat := createSym("float", Skey, tipo, 1)
	simboloCircle := createSym("circle", Smacro, nombre, 1)
	simboloRect := createSym("rect", Smacro, nombre, 1)
	builtIns := Ambito{"int": simboloInt, 
					   "float": simboloFloat, 
					   "circle": simboloCircle, 
					   "rect": simboloRect}
	return pushTabla(p, builtIns)
}


//Lexer
type Text interface {
	Get() (rune, error)
	UnGet() error
} 

type bufsrc struct {
	in io.RuneScanner
}

func (s *bufsrc) Get() (rune, error) {
	r, _, err := s.in.ReadRune()
	return r, err
}

func (s *bufsrc) UnGet() error {
	return s.in.UnreadRune()
}

var reservadas = map[string]int{
	"rect": nombre,
	"circle": nombre,
	"int": tipo, 
	"float": tipo, 
	"macro": macro, 
	"loop": loop,
}

var file string
var line int
var nerrors int

type ExprLex interface {
	Lex(lval *ExprSymType) int
	Error(e string)
}

type lex struct {
	in Text
	val []rune
}

func NewLex(t Text, fname string) *lex {
	file = fname
	line = 1
	return &lex{in: t}
}

func (l *lex) got(r rune) {
	l.val = append(l.val, r)
}

func (l *lex) getval() string {
	return string(l.val)
}

func (l *lex) skipBlanks() error {
	for {
		c, err := l.in.Get()
		if err!=nil {
			return err
		}
		if c=='#' {
			for c!='\n' {
				if c, err = l.in.Get(); err!=nil {
					return err
				}
			}
			if c=='\n' {
				line++
			}
		}
		if c=='\n' {
			line++
		}
		if !unicode.IsSpace(c) {
			l.in.UnGet()
			return nil
		}
	}
	return nil
}

func (l *lex) Error(s string)  {
    Errorf("%s near '%s'", s, l.getval())
}

func Errorf(s string, v ...interface{}) {
    fmt.Printf("%s:%d: ", file, line)
    fmt.Printf(s, v...)
    fmt.Printf("\n")
    nerrors++
    if nerrors > 5 {
        fmt.Printf("too many errors\n")
        os.Exit(1)
    }
}

func IsHexaValid(c rune) bool {
	hexa := "abcdef"
	return unicode.IsNumber(c) || strings.ContainsRune(hexa, c) 
}

func (l *lex) Lex(lval *ExprSymType) (tid int) {
    l.val = nil
    if err := l.skipBlanks(); err != nil {
        if err != io.EOF {
            Errorf("%s", err)
        }
        return 0
    }
    c, err := l.in.Get()
    if err != nil {
        Errorf("%s", err)
        return 0
    }
    l.got(c)
	switch {
    case c=='\n' || c=='+' || c=='*' || c=='/' || c=='(' || c==')' || c=='{' || c=='}' || c==';' || c==',' || c=='=' || c==':':
        lval.name = l.getval()
        return int(c)
    case c == '-':
        n, _ := l.in.Get()
        if n < '0' || n > '9' {
			l.in.UnGet()
            return '-'
        }
		l.got(n)
        fallthrough
	case c >= '0' && c <= '9':
        for {
            c, err := l.in.Get()
            if err != nil {
                Errorf("%s", err)
                return 0
            }
			
			if l.val[0] == '0' && c == 'x' {
				//Hexadecimales
				l.got(c)
				for {
					c, err := l.in.Get()
					if err != nil {
						Errorf("%s", err)
                		return 0
					}					 
					if !IsHexaValid(c) {
						l.in.UnGet()
						hexa, _ := strconv.ParseInt(string(l.val), 0, 0)
						lval.num = float64(hexa)
						return litint
					}
					l.got(unicode.ToLower(c))
				}
			}

			if c=='.' {
				l.got(c)
				for {
            		c, err := l.in.Get()
				    if err != nil {
				        Errorf("%s", err)
				        return 0
				    }
				    if !unicode.IsNumber(c) {
				        l.in.UnGet()
				        break
				    }
				    l.got(c)
				}
				lval.name = l.getval()
				n, err := strconv.ParseFloat(string(l.val), 64)
				if err != nil {
				    Errorf("%s", err)
				    return 0
				}
				lval.num = n
				return litfloat
			}
            if !unicode.IsNumber(c) {
                l.in.UnGet()
                break
            }
            l.got(c)
        }
        lval.name = l.getval()
        n, err := strconv.ParseFloat(lval.name, 64)
        if err != nil {
            Errorf("%s", err)
            return 0
        }
        lval.num = n
        return litint
	case unicode.IsLetter(c):
        for {
            c, err := l.in.Get()
            if err != nil {
                Errorf("%s", err)
                return 0
            }
            if !unicode.IsLetter(c) && !unicode.IsNumber(c) {
                l.in.UnGet()
                break
            }
            l.got(c)
        }
        lval.name = l.getval()
        b, ok := reservadas[lval.name]
        if !ok {
            return nombre
        }
        return b
    }
    Errorf("wrong input at char %c", c)
    return 0
}

func Bottupfinal(text string) bool {
	txt := &bufsrc{in: bytes.NewBufferString(text)}
	l := NewLex(txt, "prueba")
	ExprParse(l)
	fmt.Printf(PintaArbol(ast))
	fmt.Printf(PintaCodigo(ast))
	os.Exit(nerrors)
	return true
}
