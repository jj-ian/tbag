package tbagJavaTarget;

import java.util.Collection;
import java.util.HashSet;

public class Room {
	private String name;
	private String welcomeMessage;
	public Collection<Room> adjacentRooms;
	
	public Room(String name, String welcomeMessage) {
		adjacentRooms = new HashSet<Room>();
		this.name = name;
		this.welcomeMessage = welcomeMessage;
	}
	
	
	public String getName() {
		return name;
	}
	
	public String getWelcomeMessage() {
		return welcomeMessage;
	}
	
	// adding connections is 2-way
	public void addAdjacentRoom(Room otherRoom) {
		adjacentRooms.add(otherRoom);
		otherRoom.adjacentRooms.add(this);
		
	}
	
	
	
	//isotheradjacent(otherroom) hashset key - string

}
