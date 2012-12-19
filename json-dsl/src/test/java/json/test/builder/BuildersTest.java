package json.test.builder;

import static json.builder.Builders.jsonArray;
import static json.builder.Builders.jsonObject;
import static json.lang.JsonValue.ValueKind.ARRAY;
import static json.lang.JsonValue.ValueKind.NUMBER;
import static json.lang.JsonValue.ValueKind.OBJECT;
import static json.lang.JsonValue.ValueKind.STRING;
import static json.lang.JsonValue.ValueKind.TRUE;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import json.lang.JsonValue.Array;
import json.lang.JsonValue.Object;
import json.lang.exception.JsonBuildException;

import org.apache.commons.collections.CollectionUtils;
import org.junit.Test;

public class BuildersTest {

	@Test
	public void ArrayBuilderTest() {

		// Empty Array
		Array emptyArray = jsonArray().build();
		assertNotNull(emptyArray);
		assertEquals(ARRAY, emptyArray.getValueKind());
		assertTrue(CollectionUtils.isEmpty(emptyArray.getValue()));

		// Array with values
		BigDecimal number = new BigDecimal(1890734.589);
		boolean vrai = true;
		String foo = "Foo";
		Array array = jsonArray().add(number).add(vrai).add(foo).build();
		assertNotNull(array);
		assertEquals(ARRAY, array.getValueKind());
		assertTrue(array.getValue().size() == 3);
		assertTrue(number.equals(array.getValue().get(0).getValue()));
		assertTrue(TRUE.equals(array.getValue().get(1).getValueKind()));
		assertTrue(foo.equals(array.getValue().get(2).getValue()));
		assertTrue(STRING.equals(array.getValue().get(2).getValueKind()));
	}

	@Test
	public void AddArrayToArrayTest() throws JsonBuildException {
		BigDecimal number = new BigDecimal(1890734.589);
		boolean vrai = true;
		String foo = "Foo";
		Array array = jsonArray().add(number).add(vrai).add(foo).startArray()
				.add(-7894561230L).endArray().build();

		// Assert
		assertNotNull(array);
		assertEquals(ARRAY, array.getValue().get(3).getValueKind());
	}

	@Test
	public void AddObjectToArrayTest() throws JsonBuildException {
		BigDecimal number = new BigDecimal(1890734.589);
		boolean vrai = true;
		String foo = "Foo";
		Array array = jsonArray().add(number).add(vrai).add(foo).startObject()
				.add("long", -7894561230L).endObject().build();

		// Assert
		assertNotNull(array);
		assertEquals(OBJECT, array.getValue().get(3).getValueKind());
	}

	@Test
	public void ObjectBuilderTest() throws JsonBuildException {

		// Empty Object
		Object emptyObject = jsonObject().build();
		assertNotNull(emptyObject);
		assertEquals(OBJECT, emptyObject.getValueKind());
		assertTrue(CollectionUtils.isEmpty(emptyObject.getValue()));

		// Object with values
		BigDecimal number = new BigDecimal(1890734.589);
		boolean vrai = true;
		String foo = "Foo";
		Object object = jsonObject().add("number", number).add("vrai", vrai)
				.add("foo", foo).build();

		// Assert
		assertNotNull(object);
		assertEquals(OBJECT, object.getValueKind());
		assertTrue(object.getValue().size() == 3);
		assertTrue(number
				.equals(object.getValue().get(0).getValue().getValue()));
		assertTrue("number".equals(object.getValue().get(0).getName()));
		assertEquals(NUMBER, object.getValue().get(0).getValue().getValueKind());
		assertTrue((Boolean) object.getValue().get(1).getValue().getValue());
		assertTrue("vrai".equals(object.getValue().get(1).getName()));
		assertEquals(TRUE, object.getValue().get(1).getValue().getValueKind());
		assertTrue(foo.equals(object.getValue().get(2).getValue().getValue()));
		assertTrue("foo".equals(object.getValue().get(2).getName()));
		assertEquals(STRING, object.getValue().get(2).getValue().getValueKind());
	}

	@Test
	public void AddArrayToObjectTest() throws JsonBuildException {
		BigDecimal number = new BigDecimal(1890734.589);
		boolean vrai = true;
		String foo = "Foo";
		Object object = jsonObject().add("number", number).add("vrai", vrai)
				.add("foo", foo).startArray().add(-7894561230L).endArray()
				.build();

		// Assert
		assertNotNull(object);
		assertEquals(ARRAY, object.getValue().get(3).getValue().getValueKind());
	}

	@Test
	public void AddObjectToObjectTest() throws JsonBuildException {
		BigDecimal number = new BigDecimal(1890734.589);
		boolean vrai = true;
		String foo = "Foo";
		Object object = jsonObject().add("number", number).add("vrai", vrai)
				.add("foo", foo).startObject().add("long", -7894561230L)
				.endObject().build();

		// Assert
		assertNotNull(object);
		assertEquals(OBJECT, object.getValue().get(3).getValue().getValueKind());
	}

	@Test(expected = JsonBuildException.class)
	public void ObjectExceptionTest() throws JsonBuildException {
		Object object = jsonObject().add(null, true).build();
	}

	@Test
	public void BigJsonTest() throws JsonBuildException {

		Array array = jsonArray()
				// new Array
				.startArray()
					.add("String")
					.add(8192)
					.add(4096.2048D)
					.add(2888999777555666L)
					.add((String) null)
					.add(true)
					// new Array
					.startArray()
						// new Object
						.startObject()
							// new Array
							.startArray("array")
								.add("String")
								.add(8192)
								.add(4096.2048D)
								.add(2888999777555666L)
								.add((String) null)
								.add(true)
							.endArray()
							// new Object
							.startObject("object")
								.add("String", "String")
								.add("Int", 8192)
								.add("Double", 4096.2048D)
								.add("Long", 2888999777555666L)
								.add("Null String", (String) null)
								.add("True", true)
							// end Object
							.endObject()
						// end Object
						.endObject()
					// end Array
					.endArray()
				// end Array
				.endArray()
				// then Build
				.build();
	}
	
	public static void main(String[] args) throws JsonBuildException {
		
		List<String> l = new ArrayList<>();
		l.add("Toto");
		l.add(null);
		l.add("Titi");
		l.add("Tata");
		l.add("Tutu");
		
		Array array = jsonArray()
				// new Array
				.startArray()
					.add("String")
					.add(8192)
					.add(4096.2048D)
					.add(2888999777555666L)
					.add((String) null)
					.add(true)
					// new Array
					.startArray()
						// new Object
						.startObject()
							.add("col", l)
							// new Array
							.startArray("array")
								.add("String")
								.add(8192)
								.add(4096.2048D)
								.add(2888999777555666L)
								.add((String) null)
								.add(true)
							.endArray()
							// new Object
							.startObject("object")
								.add("String", "String")
								.add("Int", 8192)
								.add("Double", 4096.2048D)
								.add("Long", 2888999777555666L)
								.add("Null String", (String) null)
								.add("True", true)
							// end Object
							.endObject()
						// end Object
						.endObject()
					// end Array
					.endArray()
				// end Array
				.endArray()
				// then Build
				.build();
		System.out.println(array);
	}
	
}