import java.util.Arrays;
public class Advent2{
    public static int divisor(int[] row){
	for(int i=0; i<row.length-1; i++){
	    for(int j=i+1; j<row.length;j++){
		if(row[i]>row[j]){
		    if(row[i]%row[j]==0) return row[i]/row[j];
		} else{
		    if(row[j]%row[i]==0) return row[j]/row[i];
		}
	    }
	}
	return 0;
    }
    
    public static void main(String[] args){
	String[] nums = args[0].split(",");
	int[] numbers = new int[nums.length];
	for(int i =0; i< numbers.length; i++){
	    numbers[i] = Integer.parseInt(nums[i]);
	}
	System.out.println(Arrays.toString(numbers));
	System.out.println(divisor(numbers));
    }
}
