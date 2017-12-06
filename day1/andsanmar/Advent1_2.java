public class Advent1_2{
    public static void main(String[] args){
	int result = 0;
	int longit = args[0].length();

	for(int i = 0 ; i<longit/2; i++){
	    if(args[0].charAt(i)==args[0].charAt(i+longit/2)){
		result+=args[0].charAt(i)-'0';
	    }
	}

	System.out.println(result*2);
    }
}
