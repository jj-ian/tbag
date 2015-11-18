package tbagJavaTarget;

import java.util.HashSet;

public class Room {
	private String name;
	private String welcomeMessage;
	private HashSet<Room> adjacentRooms;
	
	public Room(String name, String welcomeMessage) {
		this.name = name;
		this.welcomeMessage = welcomeMessage;
	}
	
	
	public String getName() {
		return name;
	}
	
	public String welcomeMessage() {
		return welcomeMessage;
	}
	
	public void addAdjacentRoom(Room otherRoom) {
		adjacentRooms.add(otherRoom);
	}
	
	
	
	//isotheradjacent(otherroom) hashset key - string

}
