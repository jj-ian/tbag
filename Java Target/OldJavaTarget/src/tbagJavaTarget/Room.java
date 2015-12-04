package tbagJavaTarget;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;

public class Room {
	private String name;
	private String welcomeMessage;
	public Collection<Room> adjacentRooms;
	public ArrayList<Item> items;
	
	public Room(String name, String welcomeMessage) {
		adjacentRooms = new HashSet<Room>();
		items = new ArrayList<Item>();
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
	
	public void addItem(Item newItem) {
		items.add(newItem);
	}
	
	public void removeItem(Item itemToRemove) {
		items.remove(itemToRemove);
	}
	
	
	
	//isotheradjacent(otherroom) hashset key - string

}
