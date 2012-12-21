package json.builder.impl;

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

import json.builder.JsonArrayBuildable;
import json.builder.JsonArrayBuilder;
import json.builder.JsonBuilder;
import json.builder.JsonObjectBuildable;
import json.builder.JsonObjectBuilder;
import json.lang.JsonValue.JsonArray;
import json.lang.JsonValue.JsonBigDecimal;
import json.lang.JsonValue.JsonDouble;
import json.lang.JsonValue.JsonInteger;
import json.lang.JsonValue.JsonLong;
import json.lang.JsonValue.JsonObject;
import json.lang.JsonValue.JsonString;
import json.lang.exception.JsonBuildException;

import org.apache.commons.lang3.StringUtils;

/**
 * @author gprince
 * 
 */
public final class Builders {

    /**
     * Retourne un JsonStructureBuilder pour construire un {@link JsonObject}
     * 
     * @return un JsonStructureBuilder pour construire un {@link JsonObject}
     */
    public static final JsonObjectBuildable jsonObject() {
        return new ObjectBuildable();
    }

    /**
     * Retourne un JsonStructureBuilder pour construire un {@link JsonArray}
     * 
     * @return un JsonStructureBuilder pour construire un {@link JsonArray}
     */
    public static final JsonArrayBuildable jsonArray() {
        return new ArrayBuildable();
    }

    /**
     * @author gprince
     * 
     */
    public static class ObjectBuildable implements JsonObjectBuildable {

        @Override
        public JsonObject build() {
            return this.object;
        }

        @Override
        public JsonArrayBuilder<JsonObjectBuildable> startArray(String name) throws JsonBuildException {
            return new ArrayBuilder<>(this, name);
        }

        @Override
        public JsonObjectBuilder<JsonObjectBuildable> startObject(String name) throws JsonBuildException {
            return new ObjectBuilder<>(this, name);
        }

        @Override
        public JsonObjectBuildable add(String name, BigDecimal value) throws JsonBuildException {
            append(this.object, name, value);
            return this;
        }

        @Override
        public JsonObjectBuildable add(String name, Boolean value) throws JsonBuildException {
            append(this.object, name, value);
            return this;
        }

        @Override
        public JsonObjectBuildable add(String name, Double value) throws JsonBuildException {
            append(this.object, name, value);
            return this;
        }

        @Override
        public JsonObjectBuildable add(String name, Integer value) throws JsonBuildException {
            append(this.object, name, value);
            return this;
        }

        @Override
        public JsonObjectBuildable add(String name, Long value) throws JsonBuildException {
            append(this.object, name, value);
            return this;
        }

        @Override
        public JsonObjectBuildable add(String name, String value) throws JsonBuildException {
            append(this.object, name, value);
            return this;
        }

        @Override
        public JsonObjectBuildable add(String name, JsonObject value) throws JsonBuildException {
            append(this.object, name, value);
            return this;
        }

        @Override
        public JsonObjectBuildable add(String name, JsonArray value) throws JsonBuildException {
            append(this.object, name, value);
            return this;
        }

        @Override
        public JsonObjectBuildable merge(JsonObject object) throws JsonBuildException {
            this.object.merge(object);
            return this;
        }

        // ---------- ---------- ----------
        // PRIVATE
        // ---------- ---------- ----------

        /**
         * Private Constructor
         */
        private ObjectBuildable() {
            this.object = new JsonObject();
        }

        /**
         * Le JsonObject à construire
         */
        private final JsonObject object;
    }

    /**
     * @author gprince
     * 
     */
    public static class ArrayBuildable implements JsonArrayBuildable {

        @Override
        public JsonArray build() {
            return this.array;
        }

        @Override
        public JsonArrayBuilder<JsonArrayBuildable> startArray() {
            return new ArrayBuilder<>(this);
        }

        @Override
        public JsonObjectBuilder<JsonArrayBuildable> startObject() {
            return new ObjectBuilder<>(this);
        }

        @Override
        public JsonArrayBuildable add(BigDecimal value) {
            append(this.array, value);
            return this;
        }

        @Override
        public JsonArrayBuildable add(Boolean value) {
            append(this.array, value);
            return this;
        }

        @Override
        public JsonArrayBuildable add(Double value) {
            append(this.array, value);
            return this;
        }

        @Override
        public JsonArrayBuildable add(Integer value) {
            append(this.array, value);
            return this;
        }

        @Override
        public JsonArrayBuildable add(Long value) {
            append(this.array, value);
            return this;
        }

        @Override
        public JsonArrayBuildable add(String value) {
            append(this.array, value);
            return this;
        }

        @Override
        public JsonArrayBuildable add(JsonObject value) {
            append(this.array, value);
            return this;
        }

        @Override
        public JsonArrayBuildable add(JsonArray value) {
            append(this.array, value);
            return this;
        }

        @Override
        public JsonArrayBuildable merge(JsonArray array) {
            this.array.merge(array);
            return this;
        }

        // ---------- ---------- ----------
        // PRIVATE
        // ---------- ---------- ----------

        /**
         * Private Constructor
         */
        private ArrayBuildable() {
            this.array = new JsonArray();
        }

        /**
         * Le JsonArray à construire
         */
        private final JsonArray array;
    }

    /**
     * @author gprince
     * 
     * @param <T>
     */
    public static class ObjectBuilder<T extends JsonBuilder> implements JsonObjectBuilder<T> {

        @Override
        public JsonArrayBuilder<JsonObjectBuilder<T>> startArray(String name) throws JsonBuildException {
            return new ArrayBuilder<>(this, name);
        }

        @Override
        public JsonObjectBuilder<JsonObjectBuilder<T>> startObject(String name) throws JsonBuildException {
            return new ObjectBuilder<>(this, name);
        }

        @Override
        public JsonObjectBuilder<T> add(String name, BigDecimal value) throws JsonBuildException {
            append(object, name, value);
            return this;
        }

        @Override
        public JsonObjectBuilder<T> add(String name, Boolean value) throws JsonBuildException {
            append(object, name, value);
            return this;
        }

        @Override
        public JsonObjectBuilder<T> add(String name, Double value) throws JsonBuildException {
            append(object, name, value);
            return this;
        }

        @Override
        public JsonObjectBuilder<T> add(String name, Integer value) throws JsonBuildException {
            append(object, name, value);
            return this;
        }

        @Override
        public JsonObjectBuilder<T> add(String name, Long value) throws JsonBuildException {
            append(object, name, value);
            return this;
        }

        @Override
        public JsonObjectBuilder<T> add(String name, String value) throws JsonBuildException {
            append(object, name, value);
            return this;
        }

        @Override
        public JsonObjectBuilder<T> add(String name, JsonObject value) throws JsonBuildException {
            append(object, name, value);
            return this;
        }

        @Override
        public JsonObjectBuilder<T> add(String name, JsonArray value) throws JsonBuildException {
            append(object, name, value);
            return this;
        }

        @Override
        public JsonObjectBuilder<T> merge(JsonObject object) throws JsonBuildException {
            this.object.merge(object);
            return this;
        }

        @SuppressWarnings("unchecked")
        @Override
        public T endObject() throws JsonBuildException {
            if (ObjectBuilder.class.isInstance(this.parentBuibler)) {
                @SuppressWarnings("rawtypes")
                ObjectBuilder builder = (ObjectBuilder) this.parentBuibler;
                builder.add(this.name, this.object);
            }

            else if (ArrayBuilder.class.isInstance(this.parentBuibler)) {
                @SuppressWarnings("rawtypes")
                ArrayBuilder builder = (ArrayBuilder) this.parentBuibler;
                builder.add(this.object);
            }

            else if (ObjectBuildable.class.isInstance(this.parentBuibler)) {
                ObjectBuildable builder = (ObjectBuildable) this.parentBuibler;
                builder.add(this.name, this.object);
            }

            else if (ArrayBuildable.class.isInstance(this.parentBuibler)) {
                ArrayBuildable builder = (ArrayBuildable) this.parentBuibler;
                builder.add(this.object);
            }
            return (T) this.parentBuibler;
        }

        // ---------- ---------- ----------
        // PRIVATE
        // ---------- ---------- ----------

        /**
         * Private Constructor
         * 
         * @param parentBuibler
         *            Le builder parent
         * @param name
         *            Le nom du JsonObject à construire
         * 
         * @throws JsonBuildException
         */
        private ObjectBuilder(JsonBuilder parentBuibler, String name) throws JsonBuildException {
            // Name can not be null
            if (StringUtils.isEmpty(name)) {
                throw new JsonBuildException(BUILD_EXCEPTION_NULL_NAME, new IllegalArgumentException());
            }
            this.parentBuibler = parentBuibler;
            this.name = name;
            this.object = new JsonObject();
        }

        /**
         * Private Constructor
         * 
         * @throws JsonBuildException
         */
        private ObjectBuilder(JsonBuilder parentBuibler) {
            this.parentBuibler = parentBuibler;
            this.name = null;
            this.object = new JsonObject();
        }

        /**
         * Le builder parent
         */
        private final JsonBuilder parentBuibler;

        /**
         * Le nom du JsonObject à construire
         */
        private final String name;

        /**
         * Le JsonObject à construire
         */
        private final JsonObject object;
    }

    /**
     * @author gprince
     * 
     * @param <T>
     */
    public static class ArrayBuilder<T extends JsonBuilder> implements JsonArrayBuilder<T> {

        @Override
        public JsonArrayBuilder<JsonArrayBuilder<T>> startArray() {
            return new ArrayBuilder<>(this);
        }

        @Override
        public JsonObjectBuilder<JsonArrayBuilder<T>> startObject() {
            return new ObjectBuilder<>(this);
        }

        @Override
        public JsonArrayBuilder<T> add(BigDecimal value) {
            append(this.array, value);
            return this;
        }

        @Override
        public JsonArrayBuilder<T> add(Boolean value) {
            append(this.array, value);
            return this;
        }

        @Override
        public JsonArrayBuilder<T> add(Double value) {
            append(this.array, value);
            return this;
        }

        @Override
        public JsonArrayBuilder<T> add(Integer value) {
            append(this.array, value);
            return this;
        }

        @Override
        public JsonArrayBuilder<T> add(Long value) {
            append(this.array, value);
            return this;
        }

        @Override
        public JsonArrayBuilder<T> add(String value) {
            append(this.array, value);
            return this;
        }

        @Override
        public JsonArrayBuilder<T> add(JsonObject value) {
            append(this.array, value);
            return this;
        }

        @Override
        public JsonArrayBuilder<T> add(JsonArray value) {
            append(this.array, value);
            return this;
        }

        @Override
        public JsonArrayBuilder<T> merge(JsonArray array) {
            this.array.merge(array);
            return this;
        }

        @SuppressWarnings("unchecked")
        @Override
        public T endArray() throws JsonBuildException {
            if (ObjectBuilder.class.isInstance(this.parentBuibler)) {
                @SuppressWarnings("rawtypes")
                ObjectBuilder builder = (ObjectBuilder) this.parentBuibler;
                builder.add(this.name, this.array);
            }

            else if (ArrayBuilder.class.isInstance(this.parentBuibler)) {
                @SuppressWarnings("rawtypes")
                ArrayBuilder builder = (ArrayBuilder) this.parentBuibler;
                builder.add(this.array);
            }

            else if (ObjectBuildable.class.isInstance(this.parentBuibler)) {
                ObjectBuildable builder = (ObjectBuildable) this.parentBuibler;
                builder.add(this.name, this.array);
            }

            else if (ArrayBuildable.class.isInstance(this.parentBuibler)) {
                ArrayBuildable builder = (ArrayBuildable) this.parentBuibler;
                builder.add(this.array);
            }
            return (T) this.parentBuibler;
        }

        /**
         * Private Constructor
         * 
         * @param parentBuibler
         *            Le builder parent
         * @param name
         *            Le nom du JsonObject à construire
         * 
         * @throws JsonBuildException
         */
        private ArrayBuilder(JsonBuilder parentBuibler, String name) throws JsonBuildException {
            // Name can not be null
            if (StringUtils.isEmpty(name)) {
                throw new JsonBuildException(BUILD_EXCEPTION_NULL_NAME, new IllegalArgumentException());
            }
            this.parentBuibler = parentBuibler;
            this.name = name;
            this.array = new JsonArray();
        }

        /**
         * Private Constructor
         * 
         * @throws JsonBuildException
         */
        private ArrayBuilder(JsonBuilder parentBuibler) {
            this.parentBuibler = parentBuibler;
            this.name = null;
            this.array = new JsonArray();
        }

        /**
         * Le builder parent
         */
        private final JsonBuilder parentBuibler;

        /**
         * Le nom du JsonObject à construire
         */
        private final String name;

        /**
         * Le JsonObject à construire
         */
        private final JsonArray array;

    }

    // ---------- ---------- ----------
    // PRIVATE
    // ---------- ---------- ----------

    /**
     * Exception message
     */
    private static final String BUILD_EXCEPTION_NULL_NAME = "Name can not be null!";

    /**
     * @param object
     * @param name
     * @param value
     * @throws JsonBuildException
     */
    private static void append(JsonObject object, String name, Object value) throws JsonBuildException {
        // Name can not be null
        if (StringUtils.isEmpty(name)) {
            throw new JsonBuildException(BUILD_EXCEPTION_NULL_NAME, new IllegalArgumentException());
        }
        // BigDecimal
        if (BigDecimal.class.isInstance(value)) {
            if (value != null) {
                object.add(name, new JsonBigDecimal((BigDecimal) value));
            }

            else {
                object.add(name, NULL_BIG_DECIMAL);
            }
        }
        // Boolean
        else if (Boolean.class.isInstance(value) || Boolean.TYPE.isInstance(value)) {
            if (value != null) {
                Boolean bool = (Boolean) value;
                if (bool)
                    object.add(name, TRUE);
                else
                    object.add(name, FALSE);
            } else {
                object.add(name, NULL_BOOLEAN);
            }
        }
        // Double
        if (Double.class.isInstance(value)) {
            if (value != null) {
                object.add(name, new JsonDouble((Double) value));
            }

            else {
                object.add(name, NULL_DOUBLE);
            }
        }

        // Integer
        if (Integer.class.isInstance(value)) {
            if (value != null) {
                object.add(name, new JsonInteger((Integer) value));
            }

            else {
                object.add(name, NULL_INTEGER);
            }
        }

        // Long
        if (Long.class.isInstance(value)) {
            if (value != null) {
                object.add(name, new JsonLong((Long) value));
            }

            else {
                object.add(name, NULL_LONG);
            }
        }

        // String
        if (String.class.isInstance(value)) {
            if (value != null) {
                object.add(name, new JsonString((String) value));
            }

            else {
                object.add(name, NULL_STRING);
            }
        }

        // JsonArray
        if (JsonArray.class.isInstance(value)) {
            if (value != null) {
                object.add(name, (JsonArray) value);
            }

            else {
                object.add(name, NULL_ARRAY);
            }
        }

        // JsonObject
        if (JsonObject.class.isInstance(value)) {
            if (value != null) {
                object.add(name, (JsonObject) value);
            }

            else {
                object.add(name, NULL_OBJECT);
            }
        }
    }

    /**
     * @param array
     * @param value
     * @throws JsonBuildException
     */
    private static void append(JsonArray array, Object value) {
        // BigDecimal
        if (BigDecimal.class.isInstance(value)) {
            if (value != null) {
                array.add(new JsonBigDecimal((BigDecimal) value));
            }

            else {
                array.add(NULL_BIG_DECIMAL);
            }
        }
        // Boolean
        else if (Boolean.class.isInstance(value) || Boolean.TYPE.isInstance(value)) {
            if (value != null) {
                Boolean bool = (Boolean) value;
                if (bool)
                    array.add(TRUE);
                else
                    array.add(FALSE);
            } else {
                array.add(NULL_BOOLEAN);
            }
        }
        // Double
        if (Double.class.isInstance(value)) {
            if (value != null) {
                array.add(new JsonDouble((Double) value));
            }

            else {
                array.add(NULL_DOUBLE);
            }
        }

        // Integer
        if (Integer.class.isInstance(value)) {
            if (value != null) {
                array.add(new JsonInteger((Integer) value));
            }

            else {
                array.add(NULL_INTEGER);
            }
        }

        // Long
        if (Long.class.isInstance(value)) {
            if (value != null) {
                array.add(new JsonLong((Long) value));
            }

            else {
                array.add(NULL_LONG);
            }
        }

        // String
        if (String.class.isInstance(value)) {
            if (value != null) {
                array.add(new JsonString((String) value));
            }

            else {
                array.add(NULL_STRING);
            }
        }

        // JsonArray
        if (JsonArray.class.isInstance(value)) {
            if (value != null) {
                array.add((JsonArray) value);
            }

            else {
                array.add(NULL_ARRAY);
            }
        }

        // JsonObject
        if (JsonObject.class.isInstance(value)) {
            if (value != null) {
                array.add((JsonObject) value);
            }

            else {
                array.add(NULL_OBJECT);
            }
        }
    }
}