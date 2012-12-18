package json.builder;

import static json.builder.Builders.jsonArray;
import static json.builder.Builders.jsonObject;

import java.math.BigDecimal;

import json.lang.JsonValue.Array;
import json.lang.JsonValue.Object;

import static json.lang.JsonValue.ValueKind.ARRAY;

import static org.junit.Assert.*;

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
		Array array = jsonArray().add(number).build();
		assertNotNull(array);
		assertEquals(ARRAY, array.getValueKind());
		assertTrue(array.getValue().size() == 3);
		assertNotNull(number.equals(array.getValue().get(0)));
	}

}
