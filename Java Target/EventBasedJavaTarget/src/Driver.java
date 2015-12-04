import java.util.Arrays;
import java.util.Scanner;

public class Driver {
	
	//scans for input
	public static Scanner scanner;
	
	//global vars for event stuff
	public static Room currentRoom;
	public static String input;
	public static boolean started = false;
	public static boolean haveKey = false;
	

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
				playerGoToRoom(room2);
				System.out.println("which room?");
				promptForInput(new String[]{"south", "east"});

			}
			
			if (currentRoom == room2 && input.equalsIgnoreCase("south")) {
				playerGoToRoom(room1);
				if (haveKey == false) {
					System.out.println("there's a key here. pick it up?");
					promptForInput(new String[]{"yes", "no"});
				}
				
			}
			
			if (currentRoom == room1 && input.equalsIgnoreCase("yes")) {
				haveKey = true;
				System.out.println("which room?");
				promptForInput(new String[]{"north"});
			}
			
			if (currentRoom == room1 && input.equals("no")) {
				System.out.println("which room?");
				promptForInput(new String[]{"north"});
			}
			
			//if (currentRoom == room1 && input.)
			
		}

		
	}

	// this is what happens when u do player->room
	public static void playerGoToRoom(Room room) {
		currentRoom = room;
		System.out.println("You're in " + room.name);
		System.out.println(room.message);
		
	}
	
	public static void promptForInput(String[] acceptableInputs) {
		
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
	
	

	

}
