package EventDrivenJavaTargetPackage;
//import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;

public class Room {
	public String name;
	public String message;
	public Collection<Room> adjacentRooms;
	
	public Room() {
		adjacentRooms = new HashSet<Room>();
	}
	
	// adding connections is 2-way
	public void addAdjacentRoom(Room otherRoom) {
		adjacentRooms.add(otherRoom);
		otherRoom.adjacentRooms.add(this);
		
	}

}
