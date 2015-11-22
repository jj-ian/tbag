// interface for a function that equips an item. 
// this is kind of a hack to fake a function pointer.
package tbagJavaTarget;

import java.util.HashMap;

public interface EquipFunction {
	// the function that equips something. it takes in the playerStats HashMap
	// and modifies it.
	void equipFunction(HashMap<String, Integer> playerStats);
	

}
