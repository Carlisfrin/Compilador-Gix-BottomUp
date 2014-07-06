//Tests para el parser de gix
package bottupfinal

import "testing"

var test1 = `
#Esto es un comentario
macro line(int x, int y) {
	loop i:0,x{	#declares the variable in the scope for the loop
		circle(2, 3, y, 5);
	}
}

macro main(){
	int k;
	k = 2;
	loop i:0, 3{
		rect(i, 2*1+4, 0.4, 0xff);
	}
	loop j:0, 2{
		rect(j, k, 0.4, 0xff);
		k = j;
	}
	line(k, 4);
}`

var test2 = `
macro line(int x, int y) {
	int k;
	k = 2;
	loop i:0,x{
		circle(2, y, k);
	}
	
}

macro main() {
	int l;
	loop k: 0,4 {
		rect(2, k);
	}
	l = 2;
	circle(l, 4)		
}`

func Test_Bottupfinal_1 (t *testing.T) {
	if Bottupfinal(test1) {
		t.Log("first test passed")
	} else {
		t.Error("first test failed")
		}
}

func Test_Bottupfinal_2 (t *testing.T) {
	if Bottupfinal(test2) {
		t.Log("second test passed")
	} else {
		t.Error("second test failed")
		}
}
