import java.util.Arrays;
import java.util.Scanner;

public class Driver {
	
	//scans for input
	public static Scanner scanner;
	
	//global vars for event stuff
	public static Room currentRoom;
	public static String input;
	
	// in tbag code programmer should be able to init global vars w/ defaults
	public static boolean started = false;
	public static boolean haveKey = false;
	public static boolean room3Locked = true;
	public static boolean tryToOpenLockedDoor = false;
	public static boolean finishedLockPrompting = false;
	

	public static void main(String[] args) {
		// initialize global objects
		scanner = new Scanner(System.in);
		
		// configure rooms
		Room room1 = new Room();
		room1.name = "room1";
		room1.message = "lol im room 1";
		
		Room room2 = new Room();
		room2.name = "room2";
		room2.message = "hi im room 2";
		
		Room room3 = new Room();
		room3.name = "room3";
		room3.message = "room 3 is me";
		
		Room room4 = new Room();
		room4.name = "room4";
		room4.message = "YOOOOO U FOUND THE SECRET ROOM";
		
		// items
		
		while (true) {
			if (!started) {
				started = true;
				movePlayerToRoom(room2);
				System.out.println("which room?");
				promptForInput(new String[]{"south", "east"});

			}
			
			if (currentRoom == room2 && input.equalsIgnoreCase("south")) {
				movePlayerToRoom(room1);
				if (haveKey == false) {
					System.out.println("there's a key here. pick it up?");
					promptForInput(new String[]{"yes", "no"});
				} 
				else {
					System.out.println("which room?");
					promptForInput(new String[]{"north"});
				}
				
			}
			
			
			if (currentRoom == room1 && input.equalsIgnoreCase("yes")) {
				System.out.println("you picked up key");
				haveKey = true;
				System.out.println("which room?");
				promptForInput(new String[]{"north"});
			}
			
			if (currentRoom == room1 && input.equalsIgnoreCase("no")) {
				System.out.println("which room?");
				promptForInput(new String[]{"north"});
			}
			
			if (currentRoom == room1 && input.equalsIgnoreCase("north")) {
				movePlayerToRoom(room2);
				System.out.println("which room?");
				promptForInput(new String[]{"south", "east"});
			}
			
			if (currentRoom == room2 && input.equalsIgnoreCase("east")) {
				movePlayerToRoom(room3);
			}

			
			if (currentRoom == room3 && room3Locked == true) {
				System.out.println("there's a locked door here");
				tryToOpenLockedDoor = true;
			}
			
			if (currentRoom == room3 && room3Locked == false) {
				System.out.println("the door is unlcoked");
				
			}
			
			if (currentRoom == room3 && tryToOpenLockedDoor == true && haveKey == true) {
				System.out.println("use ur key?");
				promptForInput(new String[]{"yes", "no"});

			}
			
			if (currentRoom == room3 && tryToOpenLockedDoor == true && haveKey == false) {
				System.out.println("u don't have the key");
				tryToOpenLockedDoor = false;


			}
			
			if (currentRoom == room3 && tryToOpenLockedDoor && input.equalsIgnoreCase("yes")) {
				System.out.println("u unlocked it! woo!");
				room3Locked = false;
				tryToOpenLockedDoor = false;

			}
			
			if (currentRoom == room3 && tryToOpenLockedDoor && input.equalsIgnoreCase("no")) {
				System.out.println("u didn't try to unlock it");
				tryToOpenLockedDoor = false;

			}
			
			
			if (currentRoom == room3) {
				System.out.println("which ROOM?");
				if (room3Locked) {
					promptForInput(new String[]{"west"});
				} else {
					promptForInput(new String[]{"east","west"});

				}
				
			}

			
			if (currentRoom == room3 && input.equalsIgnoreCase("west")) {
				movePlayerToRoom(room2);
				System.out.println("which room?");
				promptForInput(new String[]{"south", "east"});			
			}
			
			if (currentRoom == room3 && input.equalsIgnoreCase("east")) {
				movePlayerToRoom(room4);
	
			}
			
			if (currentRoom == room4) {
				System.out.println("whichhhhhh room?");
				promptForInput(new String[]{"east"});		
			}
			
			if (currentRoom == room4 && input.equalsIgnoreCase("east")) {
				movePlayerToRoom(room3);
			}
			
			
			
			
		}

		
	}

	// this is what happens when u do player->room
	public static void movePlayerToRoom(Room room) {
		currentRoom = room;
		System.out.println("You're in " + room.name);
		System.out.println(room.message);
		
	}
	
	// prompts player for input and sets global var "input" to whatever player inputted, provided it's a valid input.
	// invalid inputs will be reprompted
	// boolean printAvailableInputs -- if true, it'll print the list of possible inputs. if false, it won't. enter false if you 
	// want to provide functionality like cheat codes -- where there are valid inputs that you don't want to display to player
	public static void promptForInput(String[] acceptableInputs, boolean printAvailableInputs) {
		if (printAvailableInputs == true) {
			System.out.println("available options: ");
			for (String option : acceptableInputs) {
				System.out.println(option);
			}
		}
		
		// transform to uppercase so inputs can be case insensitive
		for (int i = 0; i < acceptableInputs.length; i++) {
			acceptableInputs[i] = acceptableInputs[i].toUpperCase();
		}

		input = scanner.nextLine();
		while(!Arrays.asList(acceptableInputs).contains(input.toUpperCase())) {
			System.out.println("invalid input");
			input = scanner.nextLine();
		}				

	}
	
	// same as above, but if no printAvailableInputs argument is specified, then it defaults to true -- print available inputs 
	public static void promptForInput(String[] acceptableInputs) {
		promptForInput(acceptableInputs, true);
	}
	
	

	

}
