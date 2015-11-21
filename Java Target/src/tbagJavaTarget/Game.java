package tbagJavaTarget;

import java.util.HashMap;
import java.util.Scanner;

public class Game {

	//testing
	private Room startingRoom;
	private Scanner scanner;
	
	// stores what user input corresponds w/ which room. For example: if, when
	// user enters "3", we go to room "Dungeon", in this map will be a key, value
	// pair where key = 3, value = Dungeon
	private HashMap<Integer, Room> inputRoomMap;
	
	public static void main(String[] args) {
		
		// initialize sample rooms
		Game game = initializeSampleGame();
	
		// run the game
		game.runGame();
		
	}
	
	public Game(Room startingRoom) {
		// set instance variables
		this.startingRoom = startingRoom;
		
		// initialize other fields
		scanner = new Scanner(System.in);
		inputRoomMap = new HashMap<Integer, Room>();
	}
	
	// returns a sample Game object that you can use for gameplay
	public static Game initializeSampleGame() {
		
		// handles setup and initialization, room objects get created
		
		Room bedroom = new Room("Bedroom", "You wake up in your bed.");
		Room outside = new Room("Outside", "Congrats! You made it outside.");
		Room kitchen = new Room("Kitchen", "Cool! Looks like this is the kitchen.");
		Room bathroom = new Room("Bathroom", "Nice bathroom!");
		Room mall = new Room("Mall", "You made it to the mall! Woo!");
		
		bedroom.addAdjacentRoom(kitchen);
		bedroom.addAdjacentRoom(outside);
		bedroom.addAdjacentRoom(bathroom);
		
		bathroom.addAdjacentRoom(kitchen);
		
		outside.addAdjacentRoom(mall);
		
		return new Game(bedroom);
	}
	
	// gameplay happens here
	public void runGame() {
		Room currentRoom = this.startingRoom;
		int adjacentCounter = 1;
		
		while (/* user doesn't quit and exit condition is unsatisfied*/ true) {
			System.out.println("Current location: " + currentRoom.getName());
			System.out.println(currentRoom.getWelcomeMessage() + "\n");
		
			System.out.println("Where do you want to go?");
			
			for (Room adjacentRoom : currentRoom.adjacentRooms) {
				System.out.println(adjacentCounter + " : " + adjacentRoom.getName());
				this.inputRoomMap.put(new Integer(adjacentCounter), adjacentRoom);
				adjacentCounter++;
			}
			
			//TODO check for input type errors
			int inputInt = this.scanner.nextInt();
			System.out.println("You entered " + inputInt);
			
			Room roomToGoTo;
			
			// check that the number they entered maps to a valid room
			while ((roomToGoTo = this.inputRoomMap.get(inputInt)) == null) {
				System.out.println(inputInt + " is not a valid option.");
				inputInt = this.scanner.nextInt();
			}
			
			System.out.println("Going to room " + roomToGoTo.getName());
			System.out.println("--------------------------------------------");
			
			// re assign
			currentRoom = roomToGoTo;
			
			// reset 
			adjacentCounter = 1;
			this.inputRoomMap.clear();
			
		}
	}

}
