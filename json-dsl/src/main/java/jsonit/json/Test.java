package jsonit.json;

import java.math.BigDecimal;
import java.math.MathContext;

import jsonit.json.JsonValue.Array;
import jsonit.json.JsonValue.Null;
import jsonit.json.JsonValue.Object;

public class Test {

	/**
	 * @param args
	 */
	public static void main(String[] args) {

		Array array = new Array();

		Null<Integer> integer = new Null<>(Integer.class);

		array.add(integer);

		Array array2 = new Array();

		array.add(array2);

		array2.add(new jsonit.json.JsonValue.String("Toto"));
		array2.add(new jsonit.json.JsonValue.Integer(1492));
		array2.add(new jsonit.json.JsonValue.Number<BigDecimal>(new BigDecimal(8296734.984, MathContext.DECIMAL64),
		        BigDecimal.class));
		array2.add(new jsonit.json.JsonValue.Double(-784.56));

		Object obj1 = new Object("obj1");
		obj1.member("array2", array2);
		obj1.member("string", new jsonit.json.JsonValue.String("une string"));
		obj1.member("number", new jsonit.json.JsonValue.Number<Integer>(1492, Integer.class));
		obj1.member("double", new jsonit.json.JsonValue.Number<Double>(7784.56, Double.class));
		obj1.member("double2", new jsonit.json.JsonValue.Double(7784.56));

		array.add(obj1);

		System.out.println(array);
	}

}
