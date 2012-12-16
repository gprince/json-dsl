package jsonit.json;

import static jsonit.json.JsonValue.TRUE;
import static jsonit.json.JsonValue.FALSE;
import static jsonit.json.JsonValue.NULL_BOOLEAN;
import static jsonit.json.JsonValue.NULL_STRING;
import static jsonit.json.JsonValue.NULL_BIG_DECIMAL;
import static jsonit.json.JsonValue.NULL_INTEGER;
import static jsonit.json.JsonValue.NULL_DOUBLE;

import java.math.BigDecimal;
import java.util.Stack;

import jsonit.json.JsonValue.Array;
import jsonit.json.JsonValue.JsonStructure;

import jsonit.json.JsonValue.Object;

/**
 * @author gprince
 * 
 */
public class Json {

    public static JsonBuilder<Object> jsonObject(String name) {
        return new JsonBuilder<>(new Object(name));
    }
    
    public static JsonBuilder<Array> jsonArray() {
        return new JsonBuilder<>(new Array());
    }

    private static class JsonBuilder<T extends JsonValue<?>> implements Builder<JsonBuilder<T>>, Buildable<T> {
        
      
        @Override
        public T build() {
            return value;
        }

        @Override
        public ObjectBuilder<JsonBuilder<T>> startObject(String name) {
            // TODO Auto-generated method stub
            return null;
        }

        @Override
        public ArrayBuilder<JsonBuilder<T>> startArray() {
            // TODO Auto-generated method stub
            return null;
        }
        
        private JsonBuilder(T value) {
            this.value = value;
            this.stack = new Stack<Builder<?>>();
        }
        
        private final T value;
        
        private final Stack<? extends Builder<?>> stack;
    }

    /*
     * protected static class JsonArrayBuilder<B extends Builder<?>> implements
     * ArrayBuilder<B> {
     * 
     * @Override public ObjectBuilder<ArrayBuilder<?>> startObject(String name)
     * { // TODO Auto-generated method stub return null; }
     * 
     * @Override public ArrayBuilder<ArrayBuilder<?>> startArray() { // TODO
     * Auto-generated method stub return null; }
     * 
     * @Override public B endArray() { B parentBuilder = (B) stack.pop();
     * 
     * return parentBuilder; }
     * 
     * public ArrayBuilder<B> add(BigDecimal value) { if (value == null) {
     * this.values.add(NULL_BIG_DECIMAL); } else { this.values.add(new
     * JsonValue.BigDecimal(value)); } return this; }
     * 
     * public ArrayBuilder<B> add(Boolean value) { if (value == null) {
     * this.values.add(NULL_BOOLEAN); } else { this.values.add(value ? TRUE :
     * FALSE); } return this; }
     * 
     * public ArrayBuilder<B> add(Double value) { if (value == null) {
     * this.values.add(NULL_DOUBLE); } else { this.values.add(new
     * JsonValue.Double(value)); } return this; }
     * 
     * public ArrayBuilder<B> add(Integer value) { if (value == null) {
     * this.values.add(NULL_INTEGER); } else { this.values.add(new
     * JsonValue.Integer(value)); } return this; }
     * 
     * public ArrayBuilder<B> add(String value) { if (value == null) {
     * this.values.add(NULL_STRING); } else { this.values.add(new
     * JsonValue.String(value)); } return this; }
     * 
     * private Array values;
     * 
     * }
     */

    private Json() {
    }
}