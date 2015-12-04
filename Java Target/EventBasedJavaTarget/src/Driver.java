import java.util.Scanner;

public class Driver {
	
	//scans for input
	public static Scanner scanner;
	
	//global vars for event stuff
	public static Room currentRoom;
	public static String input;
	public static boolean started = false;
	

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		System.out.println("hello");
		
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
				playerGoToRoom(room1);
				

			}
			//System.out.println("Lol");
		}

		
	}

	// this is what happens when u do player->room1
	public static void playerGoToRoom(Room room) {
		currentRoom = room;
		System.out.println("You're in " + room.name);
		System.out.println(room.message);
		
	}

	

}
