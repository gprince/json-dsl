package json.test.builder;

import static json.builder.impl.Builders.jsonArray;
import static json.builder.impl.Builders.jsonObject;
import static json.lang.JsonValue.ValueKind.ARRAY;
import static json.lang.JsonValue.ValueKind.NUMBER;
import static json.lang.JsonValue.ValueKind.OBJECT;
import static json.lang.JsonValue.ValueKind.STRING;
import static json.lang.JsonValue.ValueKind.TRUE;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.math.BigDecimal;

import json.builder.impl.Builders.ArrayBuildable;
import json.builder.impl.Builders.IArrayBuilder;
import json.builder.impl.Builders.RootArrayBuilder;
import json.lang.JsonValue.JsonArray;
import json.lang.JsonValue.JsonObject;
import json.lang.exception.JsonBuildException;

import org.apache.commons.collections.CollectionUtils;
import org.junit.Test;

public class BuildersTest {

	@Test
	public void ArrayBuilderTest() {

		// Empty Array
		JsonArray emptyArray = jsonArray().build();
		assertNotNull(emptyArray);
		assertEquals(ARRAY, emptyArray.getValueKind());
		assertTrue(CollectionUtils.isEmpty(emptyArray.getValue()));

		// Array with values
		BigDecimal number = new BigDecimal(1890734.589);
		boolean vrai = true;
		String foo = "Foo";
		JsonArray array = jsonArray().add(number).add(vrai).add(foo).build();
		assertNotNull(array);
		assertEquals(ARRAY, array.getValueKind());
		assertTrue(array.getValue().size() == 3);
		assertTrue(number.equals(array.get(0).getValue()));
		assertTrue(TRUE.equals(array.get(1).getValueKind()));
		assertTrue(foo.equals(array.get(2).getValue()));
		assertTrue(STRING.equals(array.get(2).getValueKind()));
	}

	@Test
	public void AddArrayToArrayTest() throws JsonBuildException {
		BigDecimal number = new BigDecimal(1890734.589);
		boolean vrai = true;
		String foo = "Foo";
		JsonArray array = jsonArray().add(number).add(vrai).add(foo)
				.startArray().add(-7894561230L).endArray().build();

		// Assert
		assertNotNull(array);
		assertEquals(ARRAY, array.get(3).getValueKind());
	}

	@Test
	public void AddObjectToArrayTest() throws JsonBuildException {
		BigDecimal number = new BigDecimal(1890734.589);
		boolean vrai = true;
		String foo = "Foo";
		JsonArray array = jsonArray().add(number).add(vrai).add(foo)
				.startObject().add("long", -7894561230L).endArray().build();

		// Assert
		assertNotNull(array);
		assertEquals(OBJECT, array.get(3).getValueKind());
	}

	@Test
	public void ObjectBuilderTest() throws JsonBuildException {

		// Empty Object
		JsonObject emptyObject = jsonObject().build();
		assertNotNull(emptyObject);
		assertEquals(OBJECT, emptyObject.getValueKind());
		assertTrue(emptyObject.isEmpty());

		// Object with values
		BigDecimal number = new BigDecimal(1890734.589);
		boolean vrai = true;
		String foo = "Foo";
		JsonObject object = jsonObject().add("number", number)
				.add("vrai", vrai).add("foo", foo).build();

		// Assert
		assertNotNull(object);
		assertEquals(OBJECT, object.getValueKind());
		assertTrue(object.getValue().size() == 3);
		assertTrue(number.equals(object.get("number")));
		assertTrue(object.contains("number"));
		assertEquals(NUMBER, object.get("number").getValueKind());
		assertTrue((Boolean) object.get("vrai").getValue());
		assertTrue(object.contains("vrai"));
		assertEquals(TRUE, object.get("vrai").getValueKind());
		assertTrue(foo.equals(object.get("foo")));
		assertTrue(object.contains("foo"));
		assertEquals(STRING, object.get("foo").getValueKind());
	}

	@Test
	public void AddArrayToObjectTest() throws JsonBuildException {
		BigDecimal number = new BigDecimal(1890734.589);
		boolean vrai = true;
		String foo = "Foo";
		JsonObject object = jsonObject().add("number", number)
				.add("vrai", vrai).add("foo", foo).startArray("array")
				.add(-7894561230L).endArray().build();

		// Assert
		assertNotNull(object);
		assertEquals(ARRAY, object.get("array").getValueKind());
	}

	@Test
	public void AddObjectToObjectTest() throws JsonBuildException {
		BigDecimal number = new BigDecimal(1890734.589);
		boolean vrai = true;
		String foo = "Foo";
		JsonObject object = jsonObject().add("number", number)
				.add("vrai", vrai).add("foo", foo).startObject("array")
				.add("long", -7894561230L).endArray().build();

		// Assert
		assertNotNull(object);
		assertEquals(OBJECT, object.get("array").getValueKind());
	}

	@Test(expected = JsonBuildException.class)
	public void ObjectExceptionTest() throws JsonBuildException {
		JsonObject object = jsonObject().add(null, true).build();
	}

	@SuppressWarnings("rawtypes")
	@Test
	public void BuildTechnicTest() throws JsonBuildException {
		RootArrayBuilder builder = jsonArray();

		for (int i = 0; i < 1000; ++i) {
			builder.add(i);
			IArrayBuilder inner = builder.startArray();
			for (int j = 0; j < 1000; ++j) {
				inner.add(j);
			}
			((ArrayBuildable) inner).endArray();
		}

		// Build
		JsonArray array = builder.build();

		// Assert
		assertNotNull(array);
		assertEquals(2000, array.size());
		assertNotNull(array.get(1));
		assertEquals(ARRAY, array.get(1).getValueKind());
		assertEquals(1000, ((JsonArray) array.get(1)).size());
	}
}