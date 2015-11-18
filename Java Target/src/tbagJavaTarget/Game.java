package tbagJavaTarget;

public class Game {

	private Room startingRoom;
	
	public static void main(String[] args) {
		// TODO Auto-generated method stub
		System.out.println("hello world");
		
		//room objects get created
		Room home = new Room("Home", "You wake up in your bed.");
		Room outside = new Room("Outside", "Congrats! You made it outside.");
	}
	
	public Game(Room startingRoom) {
		this.startingRoom = startingRoom;
	}

}
