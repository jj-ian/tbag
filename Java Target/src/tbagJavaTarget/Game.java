package tbagJavaTarget;

import java.util.HashMap;
import java.util.Scanner;

public class Game {

	private Room startingRoom;
	private Scanner scanner;
	
	// stores what user input corresponds w/ which room. For example: if, when
	// user enters "3", we go to room "Dungeon", in this map will be a key, value
	// pair where key = 3, value = Dungeon
	private HashMap<Integer, Room> inputRoomMap;
	
	public static void main(String[] args) {
		
		//initialization, room objects get created
/*		Room home = new Room("Home", "You wake up in your bed.");
		Room outside = new Room("Outside", "Congrats! You made it outside.");
		
		home.addAdjacentRoom(outside);
		outside.addAdjacentRoom(home);
		*/
		Game game = initializeEverything();
	
		game.runGame();
		/*
		//gameplay happens here
		Room currentRoom = game.startingRoom;
		int adjacentCounter = 1;
		
		while ( true) {
			System.out.println("Current location: " + currentRoom.getName());
			System.out.println(currentRoom.getWelcomeMessage() + "\n");
		
			System.out.println("Where do you want to go?");
			
			for (Room adjacentRoom : currentRoom.adjacentRooms) {
				System.out.println(adjacentCounter + " : " + adjacentRoom.getName());
				game.inputRoomMap.put(new Integer(adjacentCounter), adjacentRoom);
			}
			
			//TODO check for input errors
			int inputInt = game.scanner.nextInt();
			System.out.println("You entered " + inputInt);
			
			Room roomToGoTo;
			
			// check that the number they entered maps to a valid room
			while ((roomToGoTo = game.inputRoomMap.get(inputInt)) == null) {
				System.out.println(inputInt + " is not a valid option.");
				inputInt = game.scanner.nextInt();
			}
			
			System.out.println("Going to room " + roomToGoTo.getName());
			System.out.println("--------------------------------------------");
			
			// re assign
			currentRoom = roomToGoTo;
			
			// reset 
			adjacentCounter = 1;
			game.inputRoomMap.clear();
			
		}*/
		
		
	}
	
	public Game(Room startingRoom) {
		// set instance variables
		this.startingRoom = startingRoom;
		
		// initialize other fields
		scanner = new Scanner(System.in);
		inputRoomMap = new HashMap<Integer, Room>();
	}
	
	// handles setup and initialization, room objects get created
	// returns a Game object that you can use for gameplay
	public static Game initializeEverything() {
		Room home = new Room("Home", "You wake up in your bed.");
		Room outside = new Room("Outside", "Congrats! You made it outside.");
		
		home.addAdjacentRoom(outside);
		outside.addAdjacentRoom(home);
		
		return new Game(home);
	}
	
	public void runGame() {
		//gameplay happens here
		Room currentRoom = this.startingRoom;
		int adjacentCounter = 1;
		
		while (/* user doesn't quit and exit condition is unsatisfied*/ true) {
			System.out.println("Current location: " + currentRoom.getName());
			System.out.println(currentRoom.getWelcomeMessage() + "\n");
		
			System.out.println("Where do you want to go?");
			
			for (Room adjacentRoom : currentRoom.adjacentRooms) {
				System.out.println(adjacentCounter + " : " + adjacentRoom.getName());
				this.inputRoomMap.put(new Integer(adjacentCounter), adjacentRoom);
			}
			
			//TODO check for input errors
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
