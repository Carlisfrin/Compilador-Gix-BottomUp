
//line bottupfinal.y:2
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

//line bottupfinal.y:17
type ExprSymType struct{
	yys int
	num float64
	name string
	sym *Sym
	syms []*Sym
	tree *Tree
	trees []*Tree
}

const litint = 57346
const litfloat = 57347
const nombre = 57348
const tipo = 57349
const macro = 57350
const loop = 57351

var ExprToknames = []string{
	"litint",
	"litfloat",
	"nombre",
	"tipo",
	"macro",
	"loop",
	" {",
	" }",
	" (",
	" )",
	" ,",
	" ;",
	" :",
	" =",
	" +",
	" -",
	" *",
	" /",
}
var ExprStatenames = []string{}

const ExprEofCode = 1
const ExprErrCode = 2
const ExprMaxDepth = 200

//line bottupfinal.y:315


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

//line yacctab:1
var ExprExca = []int{
	-1, 1,
	1, -1,
	-2, 0,
}

const ExprNprod = 39
const ExprPrivate = 57344

var ExprTokenNames []string
var ExprStates []string

const ExprLast = 83

var ExprAct = []int{

	38, 39, 51, 20, 60, 5, 62, 36, 47, 48,
	49, 50, 10, 16, 47, 48, 49, 50, 47, 48,
	49, 50, 26, 49, 50, 29, 6, 53, 18, 28,
	35, 64, 22, 15, 33, 27, 13, 6, 46, 34,
	28, 7, 23, 17, 3, 12, 27, 6, 54, 55,
	56, 57, 58, 43, 59, 41, 42, 6, 41, 42,
	6, 61, 9, 1, 45, 26, 63, 2, 44, 4,
	52, 37, 32, 31, 30, 14, 8, 19, 25, 24,
	21, 11, 40,
}
var ExprPact = []int{

	36, 36, -1000, 41, -1000, 29, -1000, -1000, 38, 23,
	19, 41, -1000, 33, -1000, 38, -1000, -1000, -1000, 38,
	31, -1000, 10, -1000, -1000, -1000, 22, -1000, 41, -1000,
	-8, -1000, -1000, -1000, 54, -1000, -1000, 51, 0, -1000,
	-1000, -1000, -1000, -14, 13, -1000, 0, 54, 54, 54,
	54, 54, -1000, 54, 3, 3, -1000, -1000, -10, 0,
	54, -4, -1000, 20, -1000,
}
var ExprPgo = []int{

	0, 82, 1, 81, 0, 67, 80, 79, 78, 77,
	3, 76, 12, 75, 74, 73, 72, 71, 70, 68,
	63, 62, 53,
}
var ExprR1 = []int{

	0, 20, 20, 21, 5, 1, 1, 2, 4, 4,
	4, 4, 4, 4, 3, 9, 9, 6, 12, 13,
	13, 11, 11, 10, 10, 7, 7, 7, 22, 8,
	14, 14, 18, 19, 19, 17, 17, 15, 16,
}
var ExprR2 = []int{

	0, 2, 1, 0, 10, 1, 1, 1, 3, 3,
	3, 3, 1, 1, 1, 0, 2, 2, 2, 0,
	2, 0, 3, 0, 2, 1, 3, 1, 0, 10,
	1, 1, 2, 0, 1, 0, 3, 3, 2,
}
var ExprChk = []int{

	-1000, -20, -5, 8, -5, -2, 6, 12, -11, -21,
	-12, -3, 7, 13, -13, 14, -2, 10, -12, -9,
	-10, -6, -12, 11, -7, -8, -2, 15, 9, 15,
	-14, -15, -16, 12, 17, -2, 15, -17, -4, -2,
	-1, 4, 5, -22, -19, 13, -4, 18, 19, 20,
	21, 16, -18, 14, -4, -4, -4, -4, -4, -4,
	14, -4, 10, -10, 11,
}
var ExprDef = []int{

	0, -2, 2, 0, 1, 0, 7, 21, 3, 0,
	19, 0, 14, 0, 22, 0, 18, 15, 20, 23,
	0, 16, 0, 4, 24, 25, 0, 27, 0, 17,
	0, 30, 31, 35, 0, 28, 26, 33, 38, 12,
	13, 5, 6, 0, 0, 37, 34, 0, 0, 0,
	0, 0, 36, 0, 8, 9, 10, 11, 0, 32,
	0, 0, 23, 0, 29,
}
var ExprTok1 = []int{

	1, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	12, 13, 20, 18, 14, 19, 3, 21, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 3, 16, 15,
	3, 17, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 3, 3, 10, 3, 11,
}
var ExprTok2 = []int{

	2, 3, 4, 5, 6, 7, 8, 9,
}
var ExprTok3 = []int{
	0,
}

//line yaccpar:1

/*	parser for yacc output	*/

var ExprDebug = 0

type ExprLexer interface {
	Lex(lval *ExprSymType) int
	Error(s string)
}

const ExprFlag = -1000

func ExprTokname(c int) string {
	if c > 0 && c <= len(ExprToknames) {
		if ExprToknames[c-1] != "" {
			return ExprToknames[c-1]
		}
	}
	return fmt.Sprintf("tok-%v", c)
}

func ExprStatname(s int) string {
	if s >= 0 && s < len(ExprStatenames) {
		if ExprStatenames[s] != "" {
			return ExprStatenames[s]
		}
	}
	return fmt.Sprintf("state-%v", s)
}

func Exprlex1(lex ExprLexer, lval *ExprSymType) int {
	c := 0
	char := lex.Lex(lval)
	if char <= 0 {
		c = ExprTok1[0]
		goto out
	}
	if char < len(ExprTok1) {
		c = ExprTok1[char]
		goto out
	}
	if char >= ExprPrivate {
		if char < ExprPrivate+len(ExprTok2) {
			c = ExprTok2[char-ExprPrivate]
			goto out
		}
	}
	for i := 0; i < len(ExprTok3); i += 2 {
		c = ExprTok3[i+0]
		if c == char {
			c = ExprTok3[i+1]
			goto out
		}
	}

out:
	if c == 0 {
		c = ExprTok2[1] /* unknown char */
	}
	if ExprDebug >= 3 {
		fmt.Printf("lex %U %s\n", uint(char), ExprTokname(c))
	}
	return c
}

func ExprParse(Exprlex ExprLexer) int {
	var Exprn int
	var Exprlval ExprSymType
	var ExprVAL ExprSymType
	ExprS := make([]ExprSymType, ExprMaxDepth)

	Nerrs := 0   /* number of errors */
	Errflag := 0 /* error recovery flag */
	Exprstate := 0
	Exprchar := -1
	Exprp := -1
	goto Exprstack

ret0:
	return 0

ret1:
	return 1

Exprstack:
	/* put a state and value onto the stack */
	if ExprDebug >= 4 {
		fmt.Printf("char %v in %v\n", ExprTokname(Exprchar), ExprStatname(Exprstate))
	}

	Exprp++
	if Exprp >= len(ExprS) {
		nyys := make([]ExprSymType, len(ExprS)*2)
		copy(nyys, ExprS)
		ExprS = nyys
	}
	ExprS[Exprp] = ExprVAL
	ExprS[Exprp].yys = Exprstate

Exprnewstate:
	Exprn = ExprPact[Exprstate]
	if Exprn <= ExprFlag {
		goto Exprdefault /* simple state */
	}
	if Exprchar < 0 {
		Exprchar = Exprlex1(Exprlex, &Exprlval)
	}
	Exprn += Exprchar
	if Exprn < 0 || Exprn >= ExprLast {
		goto Exprdefault
	}
	Exprn = ExprAct[Exprn]
	if ExprChk[Exprn] == Exprchar { /* valid shift */
		Exprchar = -1
		ExprVAL = Exprlval
		Exprstate = Exprn
		if Errflag > 0 {
			Errflag--
		}
		goto Exprstack
	}

Exprdefault:
	/* default state action */
	Exprn = ExprDef[Exprstate]
	if Exprn == -2 {
		if Exprchar < 0 {
			Exprchar = Exprlex1(Exprlex, &Exprlval)
		}

		/* look through exception table */
		xi := 0
		for {
			if ExprExca[xi+0] == -1 && ExprExca[xi+1] == Exprstate {
				break
			}
			xi += 2
		}
		for xi += 2; ; xi += 2 {
			Exprn = ExprExca[xi+0]
			if Exprn < 0 || Exprn == Exprchar {
				break
			}
		}
		Exprn = ExprExca[xi+1]
		if Exprn < 0 {
			goto ret0
		}
	}
	if Exprn == 0 {
		/* error ... attempt to resume parsing */
		switch Errflag {
		case 0: /* brand new error */
			Exprlex.Error("syntax error")
			Nerrs++
			if ExprDebug >= 1 {
				fmt.Printf("%s", ExprStatname(Exprstate))
				fmt.Printf("saw %s\n", ExprTokname(Exprchar))
			}
			fallthrough

		case 1, 2: /* incompletely recovered error ... try again */
			Errflag = 3

			/* find a state where "error" is a legal shift action */
			for Exprp >= 0 {
				Exprn = ExprPact[ExprS[Exprp].yys] + ExprErrCode
				if Exprn >= 0 && Exprn < ExprLast {
					Exprstate = ExprAct[Exprn] /* simulate a shift of "error" */
					if ExprChk[Exprstate] == ExprErrCode {
						goto Exprstack
					}
				}

				/* the current p has no shift on "error", pop stack */
				if ExprDebug >= 2 {
					fmt.Printf("error recovery pops state %d\n", ExprS[Exprp].yys)
				}
				Exprp--
			}
			/* there is no state on the stack with an error shift ... abort */
			goto ret1

		case 3: /* no shift yet; clobber input char */
			if ExprDebug >= 2 {
				fmt.Printf("error recovery discards %s\n", ExprTokname(Exprchar))
			}
			if Exprchar == ExprEofCode {
				goto ret1
			}
			Exprchar = -1
			goto Exprnewstate /* try again in the same state */
		}
	}

	/* reduction by production Exprn */
	if ExprDebug >= 2 {
		fmt.Printf("reduce %v in:\n\t%v\n", Exprn, ExprStatname(Exprstate))
	}

	Exprnt := Exprn
	Exprpt := Exprp
	_ = Exprpt // guard against "declared and not used"

	Exprp -= ExprR2[Exprn]
	ExprVAL = ExprS[Exprp+1]

	/* consult goto table to find next state */
	Exprn = ExprR1[Exprn]
	Exprg := ExprPgo[Exprn]
	Exprj := Exprg + ExprS[Exprp].yys + 1

	if Exprj >= ExprLast {
		Exprstate = ExprAct[Exprg]
	} else {
		Exprstate = ExprAct[Exprj]
		if ExprChk[Exprstate] != -Exprn {
			Exprstate = ExprAct[Exprg]
		}
	}
	// dummy call; replaced with literal code
	switch Exprnt {

	case 1:
		//line bottupfinal.y:43
		{ ast.Ramas = append(ast.Ramas, ExprS[Exprpt-0].tree) }
	case 2:
		//line bottupfinal.y:45
		{ ast.Ramas = append(ast.Ramas, ExprS[Exprpt-0].tree) }
	case 3:
		//line bottupfinal.y:49
		{
			simboloMacro := createSym(ExprS[Exprpt-2].name, Smacro, nombre, 1)
		  	tabla = pushSim(tabla, ExprS[Exprpt-2].name, simboloMacro)
			tabla = pushTabla(tabla, make(Ambito))
		}
	case 4:
		//line bottupfinal.y:55
		{ 
			tabla = popTabla(tabla)
			simboloMacro := createSym(ExprS[Exprpt-8].name, Smacro, nombre, 1)
			syms := []*Sym{simboloMacro}
			for i := range ExprS[Exprpt-6].syms {
				syms = append(syms, ExprS[Exprpt-6].syms[i])
			}
			macroTree := NewTree(Tmacro, syms) 
			for i := range ExprS[Exprpt-2].trees {
				macroTree.Ramas = append(macroTree.Ramas, ExprS[Exprpt-2].trees[i])
			}
			for i := range ExprS[Exprpt-1].trees {
				macroTree.Ramas = append(macroTree.Ramas, ExprS[Exprpt-1].trees[i])
			}
			ExprVAL.tree = macroTree
		}
	case 5:
		ExprVAL.num = ExprS[Exprpt-0].num
	case 6:
		ExprVAL.num = ExprS[Exprpt-0].num
	case 7:
		ExprVAL.name = ExprS[Exprpt-0].name
	case 8:
		//line bottupfinal.y:81
		{ 
			var1, err1 := strconv.ParseFloat(ExprS[Exprpt-2].name, 64)
			var2, err2 := strconv.ParseFloat(ExprS[Exprpt-0].name, 64)
			if (err1!=nil) || (err2!=nil) {
				ExprVAL.name = fmt.Sprintf("%v+%v", ExprS[Exprpt-2].name, ExprS[Exprpt-0].name) 
			}
			ExprVAL.name = fmt.Sprintf("%v", var1 + var2) 
		}
	case 9:
		//line bottupfinal.y:90
		{ 
			var1, err1 := strconv.ParseFloat(ExprS[Exprpt-2].name, 64)
			var2, err2 := strconv.ParseFloat(ExprS[Exprpt-0].name, 64)
			if (err1!=nil) || (err2!=nil) {
				ExprVAL.name = fmt.Sprintf("%v-%v", ExprS[Exprpt-2].name, ExprS[Exprpt-0].name) 
			}
			ExprVAL.name = fmt.Sprintf("%v", var1 - var2) 	
		}
	case 10:
		//line bottupfinal.y:99
		{ 
			var1, err1 := strconv.ParseFloat(ExprS[Exprpt-2].name, 64)
			var2, err2 := strconv.ParseFloat(ExprS[Exprpt-0].name, 64)
			if (err1!=nil) || (err2!=nil) {
				ExprVAL.name = fmt.Sprintf("%v*%v", ExprS[Exprpt-2].name, ExprS[Exprpt-0].name) 
			}
			ExprVAL.name = fmt.Sprintf("%v", var1 * var2) 
		}
	case 11:
		//line bottupfinal.y:108
		{ 
			var1, err1 := strconv.ParseFloat(ExprS[Exprpt-2].name, 64)
			var2, err2 := strconv.ParseFloat(ExprS[Exprpt-0].name, 64)
			if (err1!=nil) || (err2!=nil) {
				ExprVAL.name = fmt.Sprintf("%v*%v", ExprS[Exprpt-2].name, ExprS[Exprpt-0].name) 
			}
			ExprVAL.name = fmt.Sprintf("%v", var1 / var2)  
		}
	case 12:
		//line bottupfinal.y:117
		{ 
			if !IsDeclared(tabla, createSym(ExprS[Exprpt-0].name, Svar, nombre, 1)) {
				fmt.Println("symbol "+ExprS[Exprpt-0].name+" not declared")
			}
			ExprVAL.name = ExprS[Exprpt-0].name 
		}
	case 13:
		//line bottupfinal.y:124
		{ ExprVAL.name = fmt.Sprintf("%v", ExprS[Exprpt-0].num) }
	case 14:
		ExprVAL.name = ExprS[Exprpt-0].name
	case 15:
		//line bottupfinal.y:131
		{ ExprVAL.trees = nil }
	case 16:
		//line bottupfinal.y:133
		{ 
			var trees []*Tree
			for i := range ExprS[Exprpt-1].trees {
				trees = append(trees, ExprS[Exprpt-1].trees[i])
			} 
			ExprVAL.trees = append(trees, ExprS[Exprpt-0].tree)
		}
	case 17:
		//line bottupfinal.y:143
		{ ExprVAL.tree = NewTree(Tdecl, ExprS[Exprpt-1].syms) }
	case 18:
		//line bottupfinal.y:147
		{
		  simTipo := createSym(ExprS[Exprpt-1].name, Skey, nombre, 1)
		  simVar := createSym(ExprS[Exprpt-0].name, Svar, nombre, 1)
		  tabla = pushSim(tabla, ExprS[Exprpt-0].name, simVar)
		  var syms []*Sym
		  syms = append(syms, simTipo)
		  ExprVAL.syms = append(syms, simVar)
		}
	case 19:
		//line bottupfinal.y:158
		{ ExprVAL.syms = nil }
	case 20:
		//line bottupfinal.y:160
		{ ExprVAL.syms = ExprS[Exprpt-0].syms }
	case 21:
		//line bottupfinal.y:164
		{ ExprVAL.syms = nil }
	case 22:
		//line bottupfinal.y:166
		{  
			var syms []*Sym
			for i := range ExprS[Exprpt-2].syms {
				syms = append(syms, ExprS[Exprpt-2].syms[i])
			}
			for i := range ExprS[Exprpt-1].syms {
				syms = append(syms, ExprS[Exprpt-1].syms[i])
			}
			for i := range ExprS[Exprpt-0].syms {
				syms = append(syms, ExprS[Exprpt-0].syms[i])
			}
			ExprVAL.syms = syms 
		}
	case 23:
		//line bottupfinal.y:182
		{ ExprVAL.trees = nil }
	case 24:
		//line bottupfinal.y:184
		{ 
			var trees []*Tree
			for i := range ExprS[Exprpt-1].trees {
				trees = append(trees, ExprS[Exprpt-1].trees[i])
			} 
			ExprVAL.trees = append(trees, ExprS[Exprpt-0].tree)
		}
	case 25:
		ExprVAL.tree = ExprS[Exprpt-0].tree
	case 26:
		//line bottupfinal.y:195
		{
			var syms []*Sym
			simVar := createSym(ExprS[Exprpt-2].name, Svar, nombre, 1)
			syms = append(syms, simVar)
			for i := range ExprS[Exprpt-1].syms {
				syms = append(syms, ExprS[Exprpt-1].syms[i])
			}
			if len(ExprS[Exprpt-1].syms)==1 {
				ExprVAL.tree = NewTree(Tasig, syms)
			} else {
				ExprVAL.tree = NewTree(Tfunc, syms)
			}
		}
	case 27:
		//line bottupfinal.y:209
		{ ExprVAL.tree = nil }
	case 28:
		//line bottupfinal.y:213
		{
			 tabla = pushTabla(tabla, make(Ambito))
			 simVar := createSym(ExprS[Exprpt-0].name, Svar, nombre, 1)
		  	 tabla = pushSim(tabla, ExprS[Exprpt-0].name, simVar)
		}
	case 29:
		//line bottupfinal.y:219
		{	
			tabla = popTabla(tabla)
			var syms []*Sym
			var simMin *Sym
			var simMax *Sym
			simVar := createSym(ExprS[Exprpt-8].name, Svar, nombre, 1)
			_, err := strconv.ParseFloat(ExprS[Exprpt-5].name, 64)
			if err!=nil {
				simMin = createSym(ExprS[Exprpt-5].name, Svar, nombre, 1)
			} else {
				simMin = createSym(ExprS[Exprpt-5].name, Slit, nombre, 1)
			}
			_, err = strconv.ParseFloat(ExprS[Exprpt-3].name, 64)
			if err!=nil {
				simMax = createSym(ExprS[Exprpt-3].name, Svar, nombre, 1)
			} else {
				simMax = createSym(ExprS[Exprpt-3].name, Slit, nombre, 1)
			}	
			syms = append(syms, simVar)
			syms = append(syms, simMin)
			syms = append(syms, simMax)
			loopTree := NewTree(Tloop, syms)
			for i := range ExprS[Exprpt-1].trees {
				loopTree.Ramas = append(loopTree.Ramas, ExprS[Exprpt-1].trees[i])
			}
			ExprVAL.tree = loopTree
		}
	case 30:
		ExprVAL.syms = ExprS[Exprpt-0].syms
	case 31:
		ExprVAL.syms = ExprS[Exprpt-0].syms
	case 32:
		//line bottupfinal.y:253
		{
			_, err := strconv.ParseFloat(ExprS[Exprpt-0].name, 64)
			if err!=nil {
				ExprVAL.sym = createSym(ExprS[Exprpt-0].name, Svar, nombre, 1)
			} else {
				ExprVAL.sym = createSym(ExprS[Exprpt-0].name, Slit, nombre, 1)
			}
		}
	case 33:
		//line bottupfinal.y:264
		{ ExprVAL.sym = nil }
	case 34:
		//line bottupfinal.y:266
		{
			_, err := strconv.ParseFloat(ExprS[Exprpt-0].name, 64)
			if err!=nil {
				ExprVAL.sym = createSym(ExprS[Exprpt-0].name, Svar, nombre, 1)
			} else {
				ExprVAL.sym = createSym(ExprS[Exprpt-0].name, Slit, nombre, 1)
			}
		}
	case 35:
		//line bottupfinal.y:277
		{ ExprVAL.syms = nil }
	case 36:
		//line bottupfinal.y:279
		{
			var syms []*Sym
			for i := range ExprS[Exprpt-2].syms {
				syms = append(syms, ExprS[Exprpt-2].syms[i])
			}
			syms = append(syms, ExprS[Exprpt-1].sym)
			ExprVAL.syms = append(syms, ExprS[Exprpt-0].sym)
		}
	case 37:
		//line bottupfinal.y:290
		{ 
			var syms []*Sym
			for i := range ExprS[Exprpt-1].syms {
				if ExprS[Exprpt-1].syms[i]!= nil {
					syms = append(syms, ExprS[Exprpt-1].syms[i])
				}
			}
			ExprVAL.syms = syms
		}
	case 38:
		//line bottupfinal.y:302
		{ 
			var syms []*Sym
			var sim *Sym
			_, err := strconv.ParseFloat(ExprS[Exprpt-0].name, 64)
			if err!=nil {
				sim = createSym(ExprS[Exprpt-0].name, Svar, nombre, 1)
			} else {
				sim = createSym(ExprS[Exprpt-0].name, Slit, nombre, 1)
			}
			ExprVAL.syms = append(syms, sim)
		}
	}
	goto Exprstack /* stack new state and value */
}
