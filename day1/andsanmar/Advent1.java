public class Advent1{
    public static void main(String[] args){
	int result = 0;

	for(int i = 0 ; i<args[0].length()-1; i++){
	    if(args[0].charAt(i)==args[0].charAt(i+1)){
		result+=args[0].charAt(i)-'0';
	    }
	}
	if(args[0].charAt(0)==args[0].charAt(args[0].length()-1)){
	    result+=args[0].charAt(0)-'0';
	}

	System.out.println(result);
    }
}
