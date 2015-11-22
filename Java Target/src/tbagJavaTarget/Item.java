package tbagJavaTarget;

import java.util.HashMap;

public class Item {
	public String name;
	public String description;
	public EquipFunction equipFunction;
	
	public Item(String name, String description, EquipFunction equipFunction) {
		this.name = name;
		this.description = description;
		this.equipFunction = equipFunction;
	}
	
	
	/*
	public void equip(HashMap<String, Integer> playerStats) {
		//alter playerstats in some programmer defined way
		
		
		
	}*/
	
	public void equip(HashMap<String, Integer> playerStats, EquipFunction ef) {
		//alter playerstats in some programmer defined way
		ef.equipFunction(playerStats);
		System.out.println(playerStats);
		
		
	}
	
	
	@Override
	public String toString() {
		return name + " -- " + description;
	}
	
	

}
