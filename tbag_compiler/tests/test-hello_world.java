public class hello_world { 

	public class Room {
		String msg;
	}

	Room myRoom = new Room();

	Room myRoom1 = new Room();

	

	public static void main(String[] args) {
		System.out.println("hello world");
		System.out.println(add(1,2));
	}

	public static int add(int a,int b){
		return a+b*a-b/a+a;

	}

	
}