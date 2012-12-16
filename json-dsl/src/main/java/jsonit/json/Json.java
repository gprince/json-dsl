package jsonit.json;

import static jsonit.json.JsonValue.TRUE;
import static jsonit.json.JsonValue.FALSE;
import static jsonit.json.JsonValue.NULL_BOOLEAN;
import static jsonit.json.JsonValue.NULL_STRING;
import static jsonit.json.JsonValue.NULL_BIG_DECIMAL;
import static jsonit.json.JsonValue.NULL_INTEGER;
import static jsonit.json.JsonValue.NULL_LONG;
import static jsonit.json.JsonValue.NULL_DOUBLE;

import java.math.BigDecimal;
import java.util.Stack;

import jsonit.json.JsonValue.Array;
import jsonit.json.JsonValue.JsonStructure;
import jsonit.json.JsonValue.Member;

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

    public static class JsonBuilder<T extends JsonStructure<?>> implements Builder<JsonBuilder<T>>, Buildable<T> {

        private class JsonObjectBuilder<B extends Builder<?>> implements ObjectBuilder<B> {

            @Override
            public ObjectBuilder<ObjectBuilder<?>> startObject(String name) {
                // TODO Auto-generated method stub
                return null;
            }

            @Override
            public ArrayBuilder<ObjectBuilder<?>> startArray() {
                // TODO Auto-generated method stub
                return null;
            }

            @Override
            public ObjectBuilder<B> add(String name, BigDecimal value) {
                if (value != null) {
                    this.value.add(name, new jsonit.json.JsonValue.BigDecimal(value));
                } else {
                    this.value.add(name, NULL_BIG_DECIMAL);
                }
                return this;
            }

            @Override
            public ObjectBuilder<B> add(String name, Boolean value) {
                if (value != null) {
                    this.value.add(name, value ? TRUE : FALSE);
                } else {
                    this.value.add(name, NULL_BOOLEAN);
                }
                return this;
            }

            @Override
            public ObjectBuilder<B> add(String name, Double value) {
                if (value != null) {
                    this.value.add(name, new jsonit.json.JsonValue.Double(value));
                } else {
                    this.value.add(name, NULL_DOUBLE);
                }
                return this;
            }

            @Override
            public ObjectBuilder<B> add(String name, Integer value) {
                if (value != null) {
                    this.value.add(name, new jsonit.json.JsonValue.Integer(value));
                } else {
                    this.value.add(name, NULL_DOUBLE);
                }
                return this;
            }

            @Override
            public ObjectBuilder<B> add(String name, Long value) {
                if (value != null) {
                    this.value.add(name, new jsonit.json.JsonValue.Long(value));
                } else {
                    this.value.add(name, NULL_LONG);
                }
                return this;
            }

            @Override
            public ObjectBuilder<B> add(String name, String value) {
                if (value != null) {
                    this.value.add(name, new jsonit.json.JsonValue.String(value));
                } else {
                    this.value.add(name, NULL_STRING);
                }
                return this;
            }

            @SuppressWarnings("unchecked")
            @Override
            public B endObject() {
                B builder = (B) stack.pop();
                JsonStructure<?> val = builder.getValue();
                val.add(this.value);
                return builder;
            }

            private JsonObjectBuilder(String name) {
                this.value = new Object(name);
            }

            private final Object value;
        }

        @Override
        public T build() {
            return value;
        }

        @Override
        public ObjectBuilder<JsonBuilder<T>> startObject(String name) {
            stack.push(this);
            return new JsonObjectBuilder<JsonBuilder<T>>(name);
        }

        @Override
        public ArrayBuilder<JsonBuilder<T>> startArray() {
            stack.push(this);
            return null;
        }

        private JsonBuilder(T value) {
            this.value = value;
            this.stack = new Stack<Builder<?>>();
        }

        private final T value;

        private final Stack<Builder<?>> stack;
    }

    private Json() {
    }
}