package tbagJavaTarget;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Scanner;

public class Game {
	private HashMap<String, Integer> playerStats;
	private ArrayList<Item> playerItems;

	private Room startingRoom;
	private Scanner scanner;
	
	// stores what user input corresponds w/ which room. For example: if, when
	// user enters "3", we go to room "Dungeon", in this map will be a key, value
	// pair where key = 3, value = Dungeon
	private HashMap<Integer, Room> inputRoomMap;
	
	// stores what user input corresponds w/ which item
	private HashMap<Integer, Item> inputItemMap;
	
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
		inputItemMap = new HashMap<Integer, Item>();
		playerStats = new HashMap<String, Integer>();
		playerItems = new ArrayList<Item>();
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
		
		// add items to the rooms
		Item coolShirt = new Item("Drake shirt", "A nice looking shirt with Drake on it.", null);
		bedroom.addItem(coolShirt);
		
		Item knife = new Item("Knife", "A dull kitchen knife.", null);
		kitchen.addItem(knife);
		
		Item spaghetti = new Item("Spaghetti", "A big pile of saucy spaghetti.", null);
		kitchen.addItem(spaghetti);

		// create a new game, setting bedroom as the starting room
		Game game = new Game(bedroom);

		// initialize programmer defined stats
		game.playerStats.put("Health", new Integer(10));
		game.playerStats.put("Strength", 2);
		game.playerStats.put("Hunger", 5);
		game.playerStats.put("Charisma", 3 );
		
		return game;
	}
	

	// gameplay happens here
	public void runGame() {
		Room currentRoom = this.startingRoom;
		
		while (/* user doesn't quit and exit condition is unsatisfied*/ true) {
			System.out.println("Current location: " + currentRoom.getName());
			System.out.println(currentRoom.getWelcomeMessage() + "\n");
			
			//dealing w/ items in the room
			handleItems(currentRoom);
			
			// dealing w/ going to other rooms
			currentRoom = handleGoingToOtherRooms(currentRoom);
			
		}
	}
	
	public void handleItems(Room currentRoom) {
		int itemCounter = 2;
		
		if (currentRoom.items.size() > 0) {
			System.out.println("There are several items here. Which do you want to take?");

			//add additional option for "pick up no items"
			System.out.println("1 : Pick up nothing.");
			
			for (Item item : currentRoom.items) {
				System.out.println(itemCounter + " : " + item.name + " -- " + item.description);
				inputItemMap.put(new Integer(itemCounter), item);
				itemCounter++;
			}

			int inputInt = scanner.nextInt();
			//System.out.println("You entered " + inputInt);

			// input 1 is for picking up no items. Check that the input isn't 1 before proceeding
			if (inputInt == 1) {
				System.out.println("You decided not to pick up anything.\n");
			} else {
				Item itemToPickUp;

				// check that the number they entered maps to a valid item. If not, reprompt.
				while ((itemToPickUp = inputItemMap.get(inputInt)) == null) {
					System.out.println(inputInt + " is not a valid option.");
					inputInt = scanner.nextInt();
					
					// if user enters 1 in the middle, they decided to not pick up anything. 
					// exit the loop
					if (inputInt ==1) {
						System.out.println("You decided not to pick up anything.\n");
						break;
					}
				}
				
				// If they picked up something, remove the item from the room
				if (itemToPickUp != null) {
					System.out.println("Picked up " + itemToPickUp.name + ".\n");
					currentRoom.removeItem(itemToPickUp);
				}
			}

			// reset 
			itemCounter = 1;
			inputItemMap.clear();
		}
	}
	
	// handles i/o for deciding which room to go to, and returns the room to go to
	public Room handleGoingToOtherRooms(Room currentRoom) {
		int adjacentCounter = 1;
		System.out.println("Where do you want to go?");
		
		for (Room adjacentRoom : currentRoom.adjacentRooms) {
			System.out.println(adjacentCounter + " : " + adjacentRoom.getName());
			inputRoomMap.put(new Integer(adjacentCounter), adjacentRoom);
			adjacentCounter++;
		}
		
		//TODO check for input type errors
		int inputInt = scanner.nextInt();
		
		Room roomToGoTo;
		
		// check that the number they entered maps to a valid room
		while ((roomToGoTo = inputRoomMap.get(inputInt)) == null) {
			System.out.println(inputInt + " is not a valid option.");
			inputInt = scanner.nextInt();
		}
		
		System.out.println("Going to room " + roomToGoTo.getName());
		System.out.println("--------------------------------------------\n");
		
		// reset 
		adjacentCounter = 1;
		this.inputRoomMap.clear();
		
		// return the room to go to
		return roomToGoTo;
	}

}
