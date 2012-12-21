package json.lang;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * Namespace for JSON Values and Structured Types : JSON Array and JSON Object
 * 
 * 
 * 
 * @author gprince
 * 
 * @param <T>
 *            The underlying java type binded with a JsonValue
 */
public interface JsonValue<T> {

	/**
	 * The null value as a String
	 */
	static final String NULL = "null";

	/**
	 * The kinds of values
	 * 
	 * <p>
	 * A value can be an <tt>array</tt>, or <tt>false</tt>, or <tt>null</tt>, or
	 * a <tt>number</tt>, or an <tt>object</tt>, or a <tt>string</tt> in double
	 * quotes, or <tt>true</tt>
	 * </p>
	 * 
	 * @author gprince
	 * 
	 */
	enum ValueKind {
		ARRAY, FALSE, NULL, NUMBER, OBJECT, STRING, TRUE
	}

	/**
	 * <code>JsonStructure</code> is either a JsonArray or a JsonObject
	 * 
	 * <p>
	 * <code>JsonStructure</code> can can be iterated
	 * </p>
	 * 
	 * @author gprince
	 * 
	 */
	interface JsonStructure<T> extends Iterable<T> {
	}

	/**
	 * Immutable <code>JsonValue</code> that holds the boolean value
	 * <code>true</code>
	 */
	static final JsonValue<Boolean> TRUE = new JsonValue<Boolean>() {

		/**
		 * {@inheritDoc}
		 * 
		 * <p>
		 * TRUE is binded to <code>java.lang.Boolean</code>
		 * </p>
		 */
		@Override
		public Class<Boolean> getType() {
			return Boolean.class;
		}

		/**
		 * {@inheritDoc}
		 * 
		 * <p>
		 * TRUE is of kind <code>ValueKind.TRUE</code>
		 * </p>
		 */
		@Override
		public ValueKind getValueKind() {
			return ValueKind.TRUE;
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public Boolean getValue() {
			return Boolean.TRUE;
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public String toString() {
			return Boolean.TRUE.toString();
		}
	};

	/**
	 * Immutable <code>JsonValue</code> that holds the boolean value
	 * <code>false</code>
	 */
	static final JsonValue<Boolean> FALSE = new JsonValue<Boolean>() {

		/**
		 * {@inheritDoc}
		 */
		@Override
		public Class<Boolean> getType() {
			return Boolean.class;
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public ValueKind getValueKind() {
			return ValueKind.FALSE;
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public Boolean getValue() {
			return Boolean.FALSE;
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public String toString() {
			return Boolean.FALSE.toString();
		}
	};

	/**
	 * An abstract and immutable implementation of JsonValue
	 * 
	 * @author gprince
	 * 
	 * @param <T>
	 *            The type of the held value
	 */
	abstract class AbstractValue<T> implements JsonValue<T>, Serializable {

		/**
		 * Generated serialVersionUID
		 */
		private static final long serialVersionUID = 832042120875178193L;

		/**
		 * Default constructor with the held value and class
		 * 
		 * @param value
		 *            The held value
		 * @param type
		 *            The class of the held value
		 * @throws IllegalArgumentException
		 *             if the type parameter or the value parameter is null
		 */
		protected AbstractValue(T value, Class<T> type)
				throws IllegalArgumentException {
			// Type is mandatory
			if (type == null) {
				throw new IllegalArgumentException(
						ILLEGAL_ARGUMENT_EXCEPTION.replace("{0}", "type"));
			}
			// value also
			if (value == null) {
				throw new IllegalArgumentException(
						ILLEGAL_ARGUMENT_EXCEPTION.replace("{0}", "value"));
			}
			this.type = type;
			this.value = value;
		}

		/**
		 * Constructor with the class of held value. The value is always
		 * <code>null</code>
		 * 
		 * <p>
		 * This constructor creates an immutable null value
		 * </p>
		 * 
		 * @param type
		 *            The class of the held value
		 * @throws IllegalArgumentException
		 *             if the type parameter or the value parameter is null
		 */
		protected AbstractValue(Class<T> type) throws IllegalArgumentException {
			// Type is mandatory
			if (type == null) {
				throw new IllegalArgumentException(
						ILLEGAL_ARGUMENT_EXCEPTION.replace("{0}", "type"));
			}
			this.type = type;
			this.value = null;
		}

		/**
		 * Constructor with the held value
		 * 
		 * <p>
		 * The type is taken from the value
		 * </p>
		 * 
		 * @param value
		 *            the held value
		 */
		@SuppressWarnings("unchecked")
		private AbstractValue(T value) {
			this.value = value;
			this.type = (Class<? extends T>) value.getClass();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public T getValue() {
			return value;
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public Class<? extends T> getType() {
			return type;
		}

		/**
		 * Returns a string representation of this <code>JsonValue</code>.
		 * 
		 * @return a string representation of this <code>JsonValue</code>
		 */
		@Override
		public String toString() {
			if (value != null)
				return value.toString();
			return NULL;
		}

		/**
		 * An IllegalArgumentException message
		 */
		protected static final String ILLEGAL_ARGUMENT_EXCEPTION = "The {0} parameter can not be null!";

		/**
		 * The immutable class of the held value
		 */
		private final Class<? extends T> type;

		/**
		 * The immutable held value
		 */
		private final T value;
	}

	/**
	 * This class represents an immutable Array
	 * 
	 * @author gprince
	 * 
	 */
	final class JsonArray extends AbstractValue<List<JsonValue<?>>> implements
			JsonStructure<JsonValue<?>> {

		/**
		 * Generated serialVersionUID
		 */
		private static final long serialVersionUID = 8959093792887982953L;

		/**
		 * Default constructor
		 * 
		 */
		public JsonArray() {
			super(new ArrayList<JsonValue<?>>());
		}

		/**
		 * Appends the specified value to the end of this Array.
		 * 
		 * @param value
		 *            the value to be appended
		 */
		public void add(JsonValue<?> value) {
			super.value.add(value);
		}

		/**
		 * Returns the element at the specified position in this JsonArray.
		 * 
		 * @param index
		 *            index of the element to return
		 * 
		 * @return the element at the specified position in this list
		 */
		public JsonValue<?> get(int index) {
			return super.value.get(index);
		}

		/**
		 * Returns the number of elements in this JsonArray. If this JsonArray
		 * contains more than Integer.MAX_VALUE elements, returns
		 * 
		 * Integer.MAX_VALUE.
		 * 
		 * @return the number of elements in this JsonArray.
		 */
		public int size() {
			return super.value.size();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public ValueKind getValueKind() {
			return ValueKind.ARRAY;
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public Iterator<JsonValue<?>> iterator() {
			return super.value.iterator();
		}

		/**
		 * @param object
		 */
		public void merge(JsonArray array) {
			if (JsonUtils.isNotEmpty(array)) {
				super.value.addAll(array.getValue());
			}
		}
	}

	/**
	 * This class represents an immutable null value of type <code>T</code>
	 * 
	 * @author gprince
	 * 
	 * @param <T>
	 *            The type of the held value
	 */
	final class JsonNull<T> extends AbstractValue<T> {

		/**
		 * Generated serialVersionUID
		 */
		private static final long serialVersionUID = -7170548993844138203L;

		/**
		 * Default constructor with the class of the held null value
		 * 
		 * @param type
		 *            The class of the held null value
		 * @throws IllegalArgumentException
		 *             if the type parameter is null
		 */
		public JsonNull(Class<T> type) throws IllegalArgumentException {
			super(type);
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		final public ValueKind getValueKind() {
			return ValueKind.NULL;
		}
	}

	/**
	 * The immutable null Array
	 */
	static final JsonNull<JsonArray> NULL_ARRAY = new JsonNull<JsonArray>(
			JsonArray.class);

	/**
	 * The immutable null BigDecimal
	 */
	static final JsonNull<BigDecimal> NULL_BIG_DECIMAL = new JsonNull<BigDecimal>(
			BigDecimal.class);

	/**
	 * The immutable null Boolean
	 */
	static final JsonNull<Boolean> NULL_BOOLEAN = new JsonNull<Boolean>(
			Boolean.class);

	/**
	 * The immutable null Double
	 */
	static final JsonNull<Double> NULL_DOUBLE = new JsonNull<Double>(
			Double.class);

	/**
	 * The immutable null Integer
	 */
	static final JsonNull<Integer> NULL_INTEGER = new JsonNull<Integer>(
			Integer.class);
	/**
	 * The immutable null Long
	 */
	static final JsonNull<Long> NULL_LONG = new JsonNull<Long>(Long.class);

	/**
	 * The immutable null String
	 */
	static final JsonNull<String> NULL_STRING = new JsonNull<String>(
			String.class);

	/**
	 * The immutable null Object
	 */
	static final JsonNull<JsonObject> NULL_OBJECT = new JsonNull<JsonObject>(
			JsonObject.class);

	/**
	 * This class represents an immutable Number value of type <code>T</code>
	 * 
	 * @author gprince
	 * 
	 * @param <T extends Number> The type of the held value
	 */
	class JsonNumber<T extends Number> extends AbstractValue<T> {

		/**
		 * Generated serialVersionUID
		 */
		private static final long serialVersionUID = -374268257589596727L;

		/**
		 * Default constructor with the held value and class
		 * 
		 * @param value
		 *            The held value
		 * @param type
		 *            The class of the held value
		 * @throws IllegalArgumentException
		 *             if the type parameter or the value parameter is null
		 */
		public JsonNumber(T value, Class<T> type)
				throws IllegalArgumentException {
			super(value, type);
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public ValueKind getValueKind() {
			return ValueKind.NUMBER;
		}
	}

	/**
	 * Convenient class for BigDecimal value
	 * 
	 * @author gprince
	 * 
	 */
	final class JsonBigDecimal extends JsonNumber<BigDecimal> {

		/**
		 * Generated serialVersionUID
		 */
		private static final long serialVersionUID = -4796005129634434091L;

		/**
		 * Default constructor
		 * 
		 * @param value
		 *            the BigDecimal value
		 * 
		 * @throws IllegalArgumentException
		 */
		public JsonBigDecimal(BigDecimal value) throws IllegalArgumentException {
			super(value, BigDecimal.class);
		}
	}

	/**
	 * Convenient class for Double value
	 * 
	 * @author gprince
	 * 
	 */
	final class JsonDouble extends JsonNumber<Double> {

		/**
		 * Generated serialVersionUID
		 */
		private static final long serialVersionUID = 8529999113748222421L;

		/**
		 * Default constructor
		 * 
		 * @param value
		 *            the Double value
		 * 
		 * @throws IllegalArgumentException
		 */
		public JsonDouble(Double value) throws IllegalArgumentException {
			super(value, Double.class);
		}
	}

	/**
	 * Convenient class for Integer value
	 * 
	 * @author gprince
	 * 
	 */
	final class JsonInteger extends JsonNumber<Integer> {

		/**
		 * Generated serialVersionUID
		 */
		private static final long serialVersionUID = -8286327568391377698L;

		/**
		 * Default constructor
		 * 
		 * @param value
		 *            the Integer value
		 * 
		 * @throws IllegalArgumentException
		 */
		public JsonInteger(Integer value) throws IllegalArgumentException {
			super(value, Integer.class);
		}
	}

	/**
	 * Convenient class for Long value
	 * 
	 * @author gprince
	 * 
	 */
	final class JsonLong extends JsonNumber<Long> {

		/**
		 * Generated serialVersionUID
		 */
		private static final long serialVersionUID = -2587773146248648225L;

		/**
		 * Default constructor
		 * 
		 * @param value
		 *            the Long value
		 * 
		 * @throws IllegalArgumentException
		 */
		public JsonLong(Long value) throws IllegalArgumentException {
			super(value, Long.class);
		}
	}

	/**
	 * This class represents an immutable Object
	 * 
	 * @author gprince
	 * 
	 */
	final class JsonObject extends AbstractValue<Map<String, JsonValue<?>>>
			implements JsonStructure<String> {

		/**
		 * Generated serialVersionUID
		 */
		private static final long serialVersionUID = -5452807402817850441L;

		/**
		 * Default constructor
		 * 
		 * @throws IllegalArgumentException
		 *             if the value parameter is null
		 */
		public JsonObject() {
			super(new HashMap<String, JsonValue<?>>());
		}

		/**
		 * Appends the specified members to the end of this Object.
		 * 
		 * @parama name the member name
		 * @param value
		 *            the member value
		 */
		public void add(String name, JsonValue<?> value) {
			super.value.put(name, value);
		}

		/**
		 * Returns an iterator over the names of members in this Object (in
		 * proper sequence).
		 */
		@Override
		public Iterator<String> iterator() {
			return super.value.keySet().iterator();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public ValueKind getValueKind() {
			return ValueKind.OBJECT;
		}

		/**
		 * Get the JsonValue associated to the given member's name
		 * 
		 * @param name
		 *            The member name
		 * @return the associated to the given member's name
		 */
		public JsonValue<?> get(String name) {
			return super.value.get(name);
		}

		/**
		 * Returns the number of members in this JsonObject. If the JsonObject
		 * contains more than Integer.MAX_VALUE members, returns
		 * Integer.MAX_VALUE.
		 * 
		 * @return the number of members in this JsonObject
		 */
		public int size() {
			return super.value.size();
		}

		/**
		 * Returns true if this JsonObject contains no member.
		 * 
		 * @return true if this JsonObject contains no member.
		 */
		public boolean isEmpty() {
			return super.value.isEmpty();
		}

		/**
		 * Returns true if this JsonObject contains a member for the specified
		 * name.
		 * 
		 * @param name
		 *            whose presence in this JsonObject is to be tested
		 * 
		 * @return
		 */
		public boolean contains(String name) {
			return super.value.containsKey(name);
		}

		@Override
		public String toString() {
			ToStringBuilder builder = new ToStringBuilder(this,
					new ObjectToStringStyle());
			for (Map.Entry<String, JsonValue<?>> entry : super.value.entrySet()) {
				builder.append(entry.getKey()).append(":")
						.append(entry.getValue());
			}
			return builder.toString();
		}

		/**
		 * To String Style for Object
		 * 
		 * @author gprince
		 * 
		 */
		private static final class ObjectToStringStyle extends ToStringStyle {

			/**
			 * Generated serialVersionUID
			 */
			private static final long serialVersionUID = 7410446559015896437L;

			/**
			 * Default Constructor
			 */
			ObjectToStringStyle() {
				super();
				this.setUseFieldNames(false);
				this.setUseClassName(false);
				this.setUseIdentityHashCode(false);
				this.setContentStart("{");
				this.setFieldSeparator(", ");
				this.setContentEnd("}");
			}
		}

		/**
		 * @param object
		 */
		public void merge(JsonObject object) {
			if (JsonUtils.isNotEmpty(object)) {
				super.value.putAll(object.getValue());
			}
		}
	}

	/**
	 * This class represents an immutable String value
	 * 
	 * @author gprince
	 * 
	 */
	final class JsonString extends AbstractValue<String> {

		/**
		 * Generated serialVersionUID
		 */
		private static final long serialVersionUID = -6899382365388942961L;

		/**
		 * Default constructor with the held value
		 * 
		 * @param value
		 *            The held value
		 * @throws IllegalArgumentException
		 *             if the value parameter is null
		 */
		public JsonString(String value) {
			super(value, String.class);
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public ValueKind getValueKind() {
			return ValueKind.STRING;
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public String toString() {
			return STRING_DELIMITER.concat(super.toString()).concat(
					STRING_DELIMITER);
		}

		/**
		 * string delimiter
		 */
		private static final String STRING_DELIMITER = "\"";

	}

	/**
	 * Returns the {@link ValueKind} of this JsonValue
	 * 
	 * @return The kind of this JsonValue
	 */
	ValueKind getValueKind();

	/**
	 * Returns the value held by this JsonValue
	 * 
	 * @return The value held by this JsonValue
	 */
	T getValue();

	/**
	 * Returns the underlying java type binded with a JsonValue
	 * 
	 * @return the underlying java type binded with a JsonValue
	 */
	Class<? extends T> getType();
}