package json.builder;

import static json.builder.Builders.jsonArray;
import static json.builder.Builders.jsonObject;

import java.math.BigDecimal;

import json.lang.JsonValue.Array;
import json.lang.JsonValue.Object;

import static json.lang.JsonValue.ValueKind.ARRAY;
import static json.lang.JsonValue.ValueKind.OBJECT;
import static json.lang.JsonValue.ValueKind.NUMBER;
import static json.lang.JsonValue.ValueKind.STRING;
import static json.lang.JsonValue.ValueKind.TRUE;

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

        // Adding Array

        array = jsonArray().add(number).add(vrai).add(foo).startArray().add(-7894561230L).endArray().build();
    }

    @Test
    public void ObjectBuilderTest() {

        // Empty Object
        Object emptyObject = jsonObject().build();
        assertNotNull(emptyObject);
        assertEquals(OBJECT, emptyObject.getValueKind());
        assertTrue(CollectionUtils.isEmpty(emptyObject.getValue()));

        // Object with values
        BigDecimal number = new BigDecimal(1890734.589);
        boolean vrai = true;
        String foo = "Foo";
        Object object = jsonObject().add("number", number).add("vrai", vrai).add("foo", foo).build();
        assertNotNull(object);
        assertEquals(OBJECT, object.getValueKind());
        assertTrue(object.getValue().size() == 3);
        assertTrue(number.equals(object.getValue().get(0).getValue().getValue()));
        assertTrue("number".equals(object.getValue().get(0).getName()));
        assertTrue(NUMBER.equals(object.getValue().get(0).getValue().getValueKind()));
        assertTrue((Boolean) object.getValue().get(1).getValue().getValue());
        assertTrue("vrai".equals(object.getValue().get(1).getName()));
        assertTrue(TRUE.equals(object.getValue().get(1).getValue().getValueKind()));
        assertTrue(foo.equals(object.getValue().get(2).getValue().getValue()));
        assertTrue("foo".equals(object.getValue().get(2).getName()));
        assertTrue(STRING.equals(object.getValue().get(2).getValue().getValueKind()));
    }

}
