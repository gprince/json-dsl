package json.builder;

import static json.lang.JsonValue.FALSE;
import static json.lang.JsonValue.NULL_ARRAY;
import static json.lang.JsonValue.NULL_BIG_DECIMAL;
import static json.lang.JsonValue.NULL_BOOLEAN;
import static json.lang.JsonValue.NULL_DOUBLE;
import static json.lang.JsonValue.NULL_INTEGER;
import static json.lang.JsonValue.NULL_LONG;
import static json.lang.JsonValue.NULL_OBJECT;
import static json.lang.JsonValue.NULL_STRING;
import static json.lang.JsonValue.TRUE;

import java.math.BigDecimal;

import json.lang.JsonUtils;
import json.lang.JsonValue;
import json.lang.JsonValue.JsonArray;
import json.lang.JsonValue.JsonBigDecimal;
import json.lang.JsonValue.JsonDouble;
import json.lang.JsonValue.JsonInteger;
import json.lang.JsonValue.JsonLong;
import json.lang.JsonValue.JsonObject;
import json.lang.JsonValue.JsonString;
import json.lang.JsonValue.JsonStructure;
import json.lang.exception.JsonBuildException;

import org.apache.commons.lang3.StringUtils;

/**
 * Namespace for all Builders
 * 
 * @author gprince
 * 
 */
public final class Builders {

    // ---------- ---------- ----------
    // PUBLIC
    // ---------- ---------- ----------

    /**
     * @return a Builder that aims to build a JsonObject
     */
    public static RootObjectBuilder jsonObject() {
        return new RootObjectBuilder();
    }

    /**
     * @return a Builder that aims to build a JsonArray
     */
    public static RootArrayBuilder jsonArray() {
        return new RootArrayBuilder();
    }

    /**
     * Buildable is a kind of builders that finally build a
     * {@link JsonStructure}
     * 
     * @author gprince
     * 
     * @param <V>
     */
    protected interface Buildable<V extends JsonStructure<?>> {
        V build();
    }

    /**
     * @author gprince
     * 
     * @param <B>
     */
    public interface IObjectBuilder<B extends IObjectBuilder<?>> {

        B add(String name, BigDecimal value) throws JsonBuildException;

        B add(String name, Boolean value) throws JsonBuildException;

        B add(String name, Double value) throws JsonBuildException;

        B add(String name, Integer value) throws JsonBuildException;

        B add(String name, Long value) throws JsonBuildException;

        B add(String name, String value) throws JsonBuildException;

        B add(String name, JsonObject object) throws JsonBuildException;

        B add(String name, JsonArray array) throws JsonBuildException;

        B merge(JsonObject object) throws JsonBuildException;
    }

    /**
     * @author gprince
     * 
     * @param <B>
     */
    public interface IArrayBuilder<B extends IArrayBuilder<?>> {

        B add(BigDecimal value);

        B add(Boolean value);

        B add(Double value);

        B add(Integer value);

        B add(Long value);

        B add(String value);

        B add(JsonObject object) throws JsonBuildException;

        B add(JsonArray array) throws JsonBuildException;

        B merge(JsonArray array) throws JsonBuildException;

    }

    private static abstract class Builder<V extends JsonStructure<?>> {

        // ---------- ---------- ----------
        // PUBLIC
        // ---------- ---------- ----------

        protected String getName() {
            return this.name;
        }

        protected V getValue() {
            return this.value;
        }

        // ---------- ---------- ----------
        // PRIVATE
        // ---------- ---------- ----------

        private Builder(V value) {
            this.name = ANONYMOUS;
            this.value = value;
        }

        private Builder(String name, V value) {
            this.name = name;
            this.value = value;
        }

        private static final String ANONYMOUS = "[anonymous]";

        private final String name;

        private final V value;
    }

    public static abstract class RootBuilder<V extends JsonStructure<?>> extends Builder<V> implements Buildable<V> {

        // ---------- ---------- ----------
        // PUBLIC
        // ---------- ---------- ----------

        public ArrayBuilder<RootBuilder<V>> startArray() {
            return new ArrayBuilder<RootBuilder<V>>(this);
        }

        public ObjectBuilder<RootBuilder<V>> startObject() {
            return new ObjectBuilder<RootBuilder<V>>(this);
        }

        @Override
        public V build() {
            return getValue();
        }

        // ---------- ---------- ----------
        // PRIVATE
        // ---------- ---------- ----------

        private RootBuilder(V value) {
            super(value);
        }
    }

    public static class RootObjectBuilder extends RootBuilder<JsonObject> implements IObjectBuilder<RootObjectBuilder> {

        @Override
        public RootObjectBuilder add(String name, BigDecimal value) throws JsonBuildException {
            // Name can not be null
            if (StringUtils.isEmpty(name)) {
                throw new JsonBuildException(BUILD_EXCEPTION_NULL_NAME, new IllegalArgumentException());
            }

            if (value != null) {
                getValue().add(name, new JsonBigDecimal(value));
            }

            else {
                getValue().add(name, NULL_BIG_DECIMAL);
            }
            return this;
        }

        @Override
        public RootObjectBuilder add(String name, Boolean value) throws JsonBuildException {
            // Name can not be null
            if (StringUtils.isEmpty(name)) {
                throw new JsonBuildException(BUILD_EXCEPTION_NULL_NAME, new IllegalArgumentException());
            }

            if (value != null) {
                if (value)
                    getValue().add(name, TRUE);
                else
                    getValue().add(name, FALSE);
            }

            else {
                getValue().add(name, NULL_BOOLEAN);
            }
            return this;
        }

        @Override
        public RootObjectBuilder add(String name, Double value) throws JsonBuildException {
            // Name can not be null
            if (StringUtils.isEmpty(name)) {
                throw new JsonBuildException(BUILD_EXCEPTION_NULL_NAME, new IllegalArgumentException());
            }

            if (value != null) {
                getValue().add(name, new JsonDouble(value));
            }

            else {
                getValue().add(name, NULL_DOUBLE);
            }
            return this;
        }

        @Override
        public RootObjectBuilder add(String name, Integer value) throws JsonBuildException {
            // Name can not be null
            if (StringUtils.isEmpty(name)) {
                throw new JsonBuildException(BUILD_EXCEPTION_NULL_NAME, new IllegalArgumentException());
            }

            if (value != null) {
                getValue().add(name, new JsonInteger(value));
            }

            else {
                getValue().add(name, NULL_INTEGER);
            }
            return this;
        }

        @Override
        public RootObjectBuilder add(String name, Long value) throws JsonBuildException {
            // Name can not be null
            if (StringUtils.isEmpty(name)) {
                throw new JsonBuildException(BUILD_EXCEPTION_NULL_NAME, new IllegalArgumentException());
            }

            if (value != null) {
                getValue().add(name, new JsonLong(value));
            }

            else {
                getValue().add(name, NULL_LONG);
            }
            return this;
        }

        @Override
        public RootObjectBuilder add(String name, String value) throws JsonBuildException {
            // Name can not be null
            if (StringUtils.isEmpty(name)) {
                throw new JsonBuildException(BUILD_EXCEPTION_NULL_NAME, new IllegalArgumentException());
            }

            if (value != null) {
                getValue().add(name, new JsonString(value));
            }

            else {
                getValue().add(name, NULL_STRING);
            }
            return this;
        }

        @Override
        public RootObjectBuilder add(String name, JsonObject object) throws JsonBuildException {
            // Name can not be null
            if (StringUtils.isEmpty(name)) {
                throw new JsonBuildException(BUILD_EXCEPTION_NULL_NAME, new IllegalArgumentException());
            }

            if (JsonUtils.isNotEmpty(object)) {
                getValue().add(name, object);
            }

            else {
                getValue().add(name, NULL_OBJECT);
            }
            return this;
        }

        @Override
        public RootObjectBuilder add(String name, JsonArray array) throws JsonBuildException {
            // Name can not be null
            if (StringUtils.isEmpty(name)) {
                throw new JsonBuildException(BUILD_EXCEPTION_NULL_NAME, new IllegalArgumentException());
            }

            if (JsonUtils.isNotEmpty(array)) {
                getValue().add(name, array);
            }

            else {
                getValue().add(name, NULL_ARRAY);
            }
            return this;
        }

        @Override
        public RootObjectBuilder merge(JsonObject object) throws JsonBuildException {
            if (JsonUtils.isNotEmpty(object)) {
                getValue().getValue().addAll(object.getValue());
            }
            return this;
        }

        // ---------- ---------- ----------
        // PRIVATE
        // ---------- ---------- ----------

        /**
         * Private constructor
         */
        private RootObjectBuilder() {
            super(new JsonObject());
        }
    }

    public static class RootArrayBuilder extends RootBuilder<JsonArray> implements IArrayBuilder<RootArrayBuilder> {

        // ---------- ---------- ----------
        // PUBLIC
        // ---------- ---------- ----------

        @Override
        public RootArrayBuilder add(BigDecimal value) {
            if (value != null) {
                getValue().add(new JsonBigDecimal(value));
            } else {
                getValue().add(NULL_BIG_DECIMAL);
            }
            return this;
        }

        @Override
        public RootArrayBuilder add(Boolean value) {
            if (value != null) {
                if (value)
                    getValue().add(TRUE);
                else
                    getValue().add(FALSE);
            } else {
                getValue().add(NULL_BIG_DECIMAL);
            }
            return this;
        }

        @Override
        public RootArrayBuilder add(Double value) {
            if (value != null) {
                getValue().add(new JsonDouble(value));
            } else {
                getValue().add(NULL_DOUBLE);
            }
            return this;
        }

        @Override
        public RootArrayBuilder add(Integer value) {
            if (value != null) {
                getValue().add(new JsonInteger(value));
            } else {
                getValue().add(NULL_INTEGER);
            }
            return this;
        }

        @Override
        public RootArrayBuilder add(Long value) {
            if (value != null) {
                getValue().add(new JsonLong(value));
            } else {
                getValue().add(NULL_LONG);
            }
            return this;
        }

        @Override
        public RootArrayBuilder add(String value) {
            if (value != null) {
                getValue().add(new JsonString(value));
            } else {
                getValue().add(NULL_STRING);
            }
            return this;
        }

        @Override
        public RootArrayBuilder add(JsonObject object) throws JsonBuildException {
            if (JsonUtils.isNotEmpty(object)) {
                getValue().add(object);
            }

            else {
                getValue().add(NULL_OBJECT);
            }
            return this;
        }

        @Override
        public RootArrayBuilder add(JsonArray array) throws JsonBuildException {
            if (JsonUtils.isNotEmpty(array)) {
                getValue().add(array);
            }

            else {
                getValue().add(NULL_ARRAY);
            }
            return this;
        }

        @Override
        public RootArrayBuilder merge(JsonArray array) throws JsonBuildException {
            if (JsonUtils.isNotEmpty(array)) {
                getValue().getValue().addAll(array.getValue());
            }
            return this;
        }

        // ---------- ---------- ----------
        // PRIVATE
        // ---------- ---------- ----------

        /**
         * Private constructor
         */
        private RootArrayBuilder() {
            super(new JsonArray());
        }

    }

    /**
     * A abstract contract for Builders that are a children of another Builder
     * 
     * @author gprince
     * 
     * @param <T>
     * @param <V>
     */
    protected static abstract class ChildBuilder<T extends Builder<?>, V extends JsonStructure<?>> extends Builder<V> {

        // ---------- ---------- ----------
        // PUBLIC
        // ---------- ---------- ----------

        private T getParentBuilder() {
            return this.parentBuilder;
        }

        // ---------- ---------- ----------
        // PRIVATE
        // ---------- ---------- ----------

        /**
         * Build exception
         */
        private static final String BUILD_EXCEPTION = "Values other than Arrays and Objects can not be build!";

        /**
         * @return
         * @throws JsonBuildException
         */
        @SuppressWarnings("rawtypes")
        private T end() throws JsonBuildException {
            // The parent builder
            T parentBuilder = getParentBuilder();

            // Getting the value held by the parent builder
            JsonValue value = (JsonValue) parentBuilder.getValue();

            // And add the new builded value
            switch (value.getValueKind()) {
            case ARRAY:
                JsonArray array = (JsonArray) value;
                array.add((JsonValue) getValue());
                break;

            case OBJECT:
                JsonObject object = (JsonObject) value;
                object.add(getName(), (JsonValue) getValue());
                break;

            default:
                throw new JsonBuildException(BUILD_EXCEPTION);
            }

            // Finally return the parent
            return parentBuilder;
        }

        private ChildBuilder(V value) {
            super(value);
            this.parentBuilder = null;
        }

        private ChildBuilder(T parentBuilder, V value) {
            super(value);
            this.parentBuilder = parentBuilder;
        }

        private ChildBuilder(T parentBuilder, String name, V value) {
            super(name, value);
            this.parentBuilder = parentBuilder;
        }

        /**
         * The parent builder
         */
        private final T parentBuilder;
    }

    public static class ObjectBuilder<T extends Builder<?>> extends ChildBuilder<T, JsonObject> implements
            IObjectBuilder<ObjectBuilder<T>> {

        // ---------- ---------- ----------
        // PUBLIC
        // ---------- ---------- ----------

        /**
         * Start a new JsonArray within the JsonObject being built
         * 
         * @param name
         *            the member name for the new JsonArray
         * @return The builder for the new JsonArray
         */
        public ArrayBuilder<ObjectBuilder<T>> startArray(String name) {
            return new ArrayBuilder<ObjectBuilder<T>>(this, name);
        }

        /**
         * Start a new JsonObject within the JsonObject being built
         * 
         * @param name
         *            the member name for the new JsonObject
         * @return The builder for the new JsonObject
         */
        public ObjectBuilder<ObjectBuilder<T>> startObject(String name) {
            return new ObjectBuilder<ObjectBuilder<T>>(this, name);
        }

        @Override
        public ObjectBuilder<T> add(String name, BigDecimal value) throws JsonBuildException {
            // Name can not be null
            if (StringUtils.isEmpty(name)) {
                throw new JsonBuildException(BUILD_EXCEPTION_NULL_NAME, new IllegalArgumentException());
            }

            if (value != null) {
                getValue().add(name, new JsonBigDecimal(value));
            } else {
                getValue().add(name, NULL_BIG_DECIMAL);
            }
            return this;
        }

        @Override
        public ObjectBuilder<T> add(String name, Boolean value) throws JsonBuildException {
            // Name can not be null
            if (StringUtils.isEmpty(name)) {
                throw new JsonBuildException(BUILD_EXCEPTION_NULL_NAME, new IllegalArgumentException());
            }

            if (value != null) {
                if (value)
                    getValue().add(name, TRUE);
                else
                    getValue().add(name, FALSE);
            } else {
                getValue().add(name, NULL_BOOLEAN);
            }
            return this;
        }

        @Override
        public ObjectBuilder<T> add(String name, Double value) throws JsonBuildException {
            // Name can not be null
            if (StringUtils.isEmpty(name)) {
                throw new JsonBuildException(BUILD_EXCEPTION_NULL_NAME, new IllegalArgumentException());
            }

            if (value != null) {
                getValue().add(name, new JsonDouble(value));
            } else {
                getValue().add(name, NULL_DOUBLE);
            }
            return this;
        }

        @Override
        public ObjectBuilder<T> add(String name, Integer value) throws JsonBuildException {
            // Name can not be null
            if (StringUtils.isEmpty(name)) {
                throw new JsonBuildException(BUILD_EXCEPTION_NULL_NAME, new IllegalArgumentException());
            }

            if (value != null) {
                getValue().add(name, new JsonInteger(value));
            } else {
                getValue().add(name, NULL_INTEGER);
            }
            return this;
        }

        @Override
        public ObjectBuilder<T> add(String name, Long value) throws JsonBuildException {
            // Name can not be null
            if (StringUtils.isEmpty(name)) {
                throw new JsonBuildException(BUILD_EXCEPTION_NULL_NAME, new IllegalArgumentException());
            }

            if (value != null) {
                getValue().add(name, new JsonLong(value));
            } else {
                getValue().add(name, NULL_LONG);
            }
            return this;
        }

        @Override
        public ObjectBuilder<T> add(String name, String value) throws JsonBuildException {
            // Name can not be null
            if (StringUtils.isEmpty(name)) {
                throw new JsonBuildException(BUILD_EXCEPTION_NULL_NAME, new IllegalArgumentException());
            }

            if (value != null) {
                getValue().add(name, new JsonString(value));
            } else {
                getValue().add(name, NULL_STRING);
            }
            return this;
        }

        @Override
        public ObjectBuilder<T> add(String name, JsonObject object) throws JsonBuildException {
            // Name can not be null
            if (StringUtils.isEmpty(name)) {
                throw new JsonBuildException(BUILD_EXCEPTION_NULL_NAME, new IllegalArgumentException());
            }

            if (JsonUtils.isNotEmpty(object)) {
                getValue().add(name, object);
            }

            else {
                getValue().add(name, NULL_OBJECT);
            }
            return this;
        }

        @Override
        public ObjectBuilder<T> add(String name, JsonArray array) throws JsonBuildException {
            // Name can not be null
            if (StringUtils.isEmpty(name)) {
                throw new JsonBuildException(BUILD_EXCEPTION_NULL_NAME, new IllegalArgumentException());
            }

            if (JsonUtils.isNotEmpty(array)) {
                getValue().add(name, array);
            }

            else {
                getValue().add(name, NULL_ARRAY);
            }
            return this;
        }

        @Override
        public ObjectBuilder<T> merge(JsonObject object) throws JsonBuildException {
            if (JsonUtils.isNotEmpty(object)) {
                getValue().getValue().addAll(object.getValue());
            }
            return this;
        }

        /**
         * @return
         * @throws JsonBuildException
         */
        public T endObject() throws JsonBuildException {
            return super.end();
        }

        // ---------- ---------- ----------
        // PRIVATE
        // ---------- ---------- ----------

        /**
         * Private constructor
         * 
         * @param parentBuilder
         *            The parent builder
         */
        private ObjectBuilder(T parentBuilder) {
            super(parentBuilder, new JsonObject());
        }

        /**
         * Private constructor
         * 
         * @param parentBuilder
         *            The parent builder
         * @param name
         *            The member name
         */
        private ObjectBuilder(T parentBuilder, String name) {
            super(parentBuilder, name, new JsonObject());
        }
    }

    public static class ArrayBuilder<T extends Builder<?>> extends ChildBuilder<T, JsonArray> implements
            IArrayBuilder<ArrayBuilder<T>> {

        // ---------- ---------- ----------
        // PUBLIC
        // ---------- ---------- ----------

        public ArrayBuilder<ArrayBuilder<T>> startArray() {
            return new ArrayBuilder<ArrayBuilder<T>>(this);
        }

        public ObjectBuilder<ArrayBuilder<T>> startObject() {
            return new ObjectBuilder<ArrayBuilder<T>>(this);
        }

        @Override
        public ArrayBuilder<T> add(BigDecimal value) {
            if (value != null) {
                getValue().add(new JsonBigDecimal(value));
            } else {
                getValue().add(NULL_BIG_DECIMAL);
            }
            return this;
        }

        @Override
        public ArrayBuilder<T> add(Boolean value) {
            if (value != null) {
                if (value)
                    getValue().add(TRUE);
                else
                    getValue().add(FALSE);
            } else {
                getValue().add(NULL_BIG_DECIMAL);
            }
            return this;
        }

        @Override
        public ArrayBuilder<T> add(Double value) {
            if (value != null) {
                getValue().add(new JsonDouble(value));
            } else {
                getValue().add(NULL_DOUBLE);
            }
            return this;
        }

        @Override
        public ArrayBuilder<T> add(Integer value) {
            if (value != null) {
                getValue().add(new JsonInteger(value));
            } else {
                getValue().add(NULL_INTEGER);
            }
            return this;
        }

        @Override
        public ArrayBuilder<T> add(Long value) {
            if (value != null) {
                getValue().add(new JsonLong(value));
            } else {
                getValue().add(NULL_LONG);
            }
            return this;
        }

        @Override
        public ArrayBuilder<T> add(String value) {
            if (value != null) {
                getValue().add(new JsonString(value));
            } else {
                getValue().add(NULL_STRING);
            }
            return this;
        }

        @Override
        public ArrayBuilder<T> add(JsonObject object) throws JsonBuildException {
            if (JsonUtils.isNotEmpty(object)) {
                getValue().add(object);
            }

            else {
                getValue().add(NULL_OBJECT);
            }
            return this;
        }

        @Override
        public ArrayBuilder<T> add(JsonArray array) throws JsonBuildException {
            if (JsonUtils.isNotEmpty(array)) {
                getValue().add(array);
            }

            else {
                getValue().add(NULL_ARRAY);
            }
            return this;
        }

        @Override
        public ArrayBuilder<T> merge(JsonArray array) throws JsonBuildException {
            if (JsonUtils.isNotEmpty(array)) {
                getValue().getValue().addAll(array.getValue());
            }
            return this;
        }

        /**
         * @return
         * @throws JsonBuildException
         */
        public T endArray() throws JsonBuildException {
            return super.end();
        }

        // ---------- ---------- ----------
        // PRIVATE
        // ---------- ---------- ----------

        /**
         * Private constructor
         * 
         * @param parentBuilder
         *            The parent Builder
         */
        private ArrayBuilder(T parentBuilder) {
            super(parentBuilder, new JsonArray());
        }

        /**
         * Private constructor
         * 
         * @param parentBuilder
         *            The parent Builder
         * @param name
         *            The memebre name
         */
        private ArrayBuilder(T parentBuilder, String name) {
            super(parentBuilder, name, new JsonArray());
        }
    }

    /**
     * Exception message
     */
    private static final String BUILD_EXCEPTION_NULL_NAME = "Name can not be null!";
}