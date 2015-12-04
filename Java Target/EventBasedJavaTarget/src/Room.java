//import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;

public class Room {
	public String name;
	public String message;
	public Collection<Room> adjacentRooms;
	//public ArrayList<Item> items;
	
	public Room() {
		adjacentRooms = new HashSet<Room>();

	}
	/*
	public Room(String name, String welcomeMessage) {
		adjacentRooms = new HashSet<Room>();
	//	items = new ArrayList<Item>();
		this.name = name;
		this.welcomeMessage = welcomeMessage;
		
	}*/
	

	// adding connections is 2-way
	public void addAdjacentRoom(Room otherRoom) {
		adjacentRooms.add(otherRoom);
		otherRoom.adjacentRooms.add(this);
		
	}
	/*
	public void addItem(Item newItem) {
		items.add(newItem);
	}
	
	public void removeItem(Item itemToRemove) {
		items.remove(itemToRemove);
	}*/
	
	
	
	//isotheradjacent(otherroom) hashset key - string

}
