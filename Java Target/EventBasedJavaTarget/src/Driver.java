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
			
			/* designate starting room */
			if (!started) {
				started = true;
				movePlayerToRoom(room2);
			}
			
			
			/* ROOM 2 BEGINS --------------------------------------------- */
			/* boilerplate code for room2 BEGINS ------------------------- */
			
			if (currentRoom == room2) {
				System.out.println("which room?");
				promptForInput(new String[]{"south", "east"});
			}
			
			if (currentRoom == room2 && input.equalsIgnoreCase("south")) {
				movePlayerToRoom(room1);
				
			}
			
			if (currentRoom == room2 && input.equalsIgnoreCase("east")) {
				movePlayerToRoom(room3);
			}
			
			/* boilerplate code for room2 ENDS -------------------------- */
			/* ROOM 2 ENDS ---------------------------------------------- */
			
			
			/* ROOM 1 BEGINS --------------------------------------------- */
			/* unique code for room1 BEGINS ------------------------------ */
			
			if (currentRoom == room1 && haveKey == false) {
				System.out.println("there's a key here. pick it up?");
				promptForInput(new String[]{"yes", "no"});
			}
			
			if (currentRoom == room1 && input.equalsIgnoreCase("yes")) {
				System.out.println("you picked up key");
				haveKey = true;
			}
			
			/* unique code for room1 ENDS -------------------------------- */
			
			/* boilerplate code for room1 BEGINS ------------------------- */
			
			if (currentRoom == room1) {
				System.out.println("which room?");
				promptForInput(new String[]{"north"});			}
			
			if (currentRoom == room1 && input.equalsIgnoreCase("north")) {
				movePlayerToRoom(room2);
			}
			
			/* boilerplate code for room1 ENDS -------------------------- */
			/* ROOM 1 ENDS ---------------------------------------------- */

			
			/* ROOM 3 BEGINS --------------------------------------------- */
			/* unique code for room3 BEGINS ------------------------------ */
			
			if (currentRoom == room3 && room3Locked == true) {
				System.out.println("there's a locked door here");
				tryToOpenLockedDoor = true;
			}
			
			if (currentRoom == room3 && room3Locked == false) {
				System.out.println("the door is unlocked");
				
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
			
			/* boilerplate code behavior for room3 is overridden by the programmer here. 
			 * Usually here it would prompt player for all adjacent rooms, but the programmer 
			 * needs to override that here because one of the adjacent rooms may be locked.
			 * Then, there is no boilerplate code for room3; it's all unique.
			 * 
			 * So to make this easy for programmer on the tbag side, we should have a library function 
			 * called "prompt_for_adjacent_rooms(room3)" that programmer can call at the end of 
			 * their room3 code. prompt_for_adjacent_rooms(room3) will generate all boilerplate 
			 * move-to-adjacent-room code for room3.
			 * 
			 * If programmer does not want the default move-to-adjacent-room behavior,
			 * they can simply replace the prompt_for_adjacent_rooms() call with their own 
			 * handlers instead.
			 */
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
			}
			
			if (currentRoom == room3 && input.equalsIgnoreCase("east")) {
				movePlayerToRoom(room4);
	
			}
			
			/* unique code for room3 ENDS -------------------------------- */
			/* ROOM 3 ENDS ---------------------------------------------- */

			
			/* ROOM 4 BEGINS --------------------------------------------- */
			/* unique code for room4 BEGINS ------------------------------ */
			// this demonstrates how a cheat code can be implemented
			if (currentRoom == room4) {
				System.out.println("how r u today?");
				

				System.out.println("available options: ");
				System.out.println("good");
				System.out.println("bad");


				// this call of the special 2-argument promptForInput() will prompt the user
				// for input, but not print out the available inputs. This allows for the 
				// cheat code "xyzzy" to be an allowed input, but not be displayed to player.
				promptForInput(new String[]{"good", "bad", "xyzzy"}, false);
	
			}
			
			if (currentRoom == room4 && input.equalsIgnoreCase("xyzzy")) {
				System.out.println("SICK!!!!!!!!!!! U FOUND THE SPECIAL CHEAT!! IT TELEPORTS U BACK TO ROOM1!!");
				movePlayerToRoom(room1);
			}
			
			/* boilerplate code for room4 BEGINS ------------------------- */
			if (currentRoom == room4) {
				System.out.println("which room?");
				promptForInput(new String[]{"west"});		
			}
			
			if (currentRoom == room4 && input.equalsIgnoreCase("west")) {
				movePlayerToRoom(room3);
			}
			/* boilerplate code for room4 ENDS -------------------------- */
			/* ROOM 4 ENDS ---------------------------------------------- */
			
			
		}

		
	}

	// this is what happens when u do player->room
	public static void movePlayerToRoom(Room room) {
		currentRoom = room;
		System.out.println("You're in " + room.name);
		System.out.println(room.message);
		
	}
	
	// Prompts player for input and sets global var "input" to whatever player submitted, provided it's a valid input. 
	// If invalid inputs are entered, it'll reprompt until player enters a valid input.
	
	// Arguments:
	// String[] acceptableInputs -- the list of acceptable inputs
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

		// loop until player enters valid input
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
