package jsonit.json;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * Namespace for Json values and structured types
 * 
 * 
 * @author gprince
 * 
 * @param <T>
 *            The underlying java type binded with the Json Value
 */
public interface JsonValue<T> {

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
     * @author gprince
     * 
     */
    interface JsonStructure<T> extends Iterable<T> {

        void add(Object object);

        void add(Array array);
    }

    /**
     * Immutable <code>JsonValue</code> holding the boolean value
     * <code>true</code>
     */
    static final JsonValue<Boolean> TRUE = new JsonValue<Boolean>() {

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
        public java.lang.String toString() {
            return Boolean.TRUE.toString();
        }
    };

    /**
     * Immutable <code>JsonValue</code> holding the boolean value
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
        public java.lang.String toString() {
            return Boolean.FALSE.toString();
        }
    };

    /**
     * An abstract implementation of JsonValue
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
         * The null value as a String
         */
        public static final java.lang.String NULL = "null";

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
        protected AbstractValue(T value, Class<T> type) throws IllegalArgumentException {
            if (type == null) {
                throw new IllegalArgumentException(ILLEGAL_ARGUMENT_EXCEPTION.replace("{0}", "type"));
            }
            if (value == null) {
                throw new IllegalArgumentException(ILLEGAL_ARGUMENT_EXCEPTION.replace("{0}", "value"));
            }
            this.type = type;
            this.value = value;
        }

        /**
         * Constructor with the class of held value
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
            if (type == null) {
                throw new IllegalArgumentException(ILLEGAL_ARGUMENT_EXCEPTION.replace("{0}", "type"));
            }
            this.type = type;
            this.value = null;
        }

        /**
         * Constructor with the held value
         * 
         * @param value
         *            the held value
         */
        @SuppressWarnings("unchecked")
        public AbstractValue(T value) {
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
        public java.lang.String toString() {
            if (value != null)
                return value.toString();
            return NULL;
        }

        /**
         * An IllegalArgumentException message
         */
        protected static final java.lang.String ILLEGAL_ARGUMENT_EXCEPTION = "The {0} parameter can not be null!";

        /**
         * The immutable class of the held value
         */
        protected final Class<? extends T> type;

        /**
         * The immutable held value
         */
        protected final T value;
    }

    /**
     * This class represents an immutable Json Array
     * 
     * @author gprince
     * 
     */
    final class Array extends AbstractValue<List<JsonValue<?>>> implements JsonStructure<JsonValue<?>> {

        /**
         * Generated serialVersionUID
         */
        private static final long serialVersionUID = 8959093792887982953L;

        /**
         * Default constructor
         * 
         */
        public Array() {
            super(new ArrayList<JsonValue<?>>(0));
        }

        /**
         * Appends the specified value to the end of this Array.
         * 
         * @param value
         *            the value to be appended
         */
        public void add(JsonValue<?> value) {
            this.value.add(value);
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
            return this.value.iterator();
        }

        @Override
        public void add(Object object) {
            this.value.add(object);
        }

        @Override
        public void add(Array array) {
            this.value.add(array);
        }
    }

    /**
     * This class represents an immutable Json null value of type <code>T</code>
     * 
     * @author gprince
     * 
     * @param <T>
     *            The type of the held value
     */
    final class Null<T> extends AbstractValue<T> {

        /**
         * Generated serialVersionUID
         */
        private static final long serialVersionUID = -7170548993844138203L;

        /**
         * Default constructor with the class of the held Json null value
         * 
         * @param type
         *            The class of the held null value
         * @throws IllegalArgumentException
         *             if the type parameter is null
         */
        public Null(Class<T> type) throws IllegalArgumentException {
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
     * The immutable null BigDecimal
     */
    static final Null<java.math.BigDecimal> NULL_BIG_DECIMAL = new Null<java.math.BigDecimal>(java.math.BigDecimal.class);

    /**
     * The immutable null Boolean
     */
    static final Null<java.lang.Boolean> NULL_BOOLEAN = new Null<java.lang.Boolean>(java.lang.Boolean.class);

    /**
     * The immutable null Double
     */
    static final Null<java.lang.Double> NULL_DOUBLE = new Null<java.lang.Double>(java.lang.Double.class);

    /**
     * The immutable null Integer
     */
    static final Null<java.lang.Integer> NULL_INTEGER = new Null<java.lang.Integer>(java.lang.Integer.class);
    /**
     * The immutable null Long
     */
    static final Null<java.lang.Long> NULL_LONG = new Null<java.lang.Long>(java.lang.Long.class);

    /**
     * The immutable null String
     */
    static final Null<java.lang.String> NULL_STRING = new Null<java.lang.String>(java.lang.String.class);

    /**
     * This class represents an immutable Json Number value of type
     * <code>T</code>
     * 
     * @author gprince
     * 
     * @param <T extends java.lang.Number> The type of the held value
     */
    class Number<T extends java.lang.Number> extends AbstractValue<T> {

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
        public Number(T value, Class<T> type) throws IllegalArgumentException {
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
    final class BigDecimal extends Number<java.math.BigDecimal> {

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
        public BigDecimal(java.math.BigDecimal value) throws IllegalArgumentException {
            super(value, java.math.BigDecimal.class);
        }
    }

    /**
     * Convenient class for Double value
     * 
     * @author gprince
     * 
     */
    final class Double extends Number<java.lang.Double> {

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
        public Double(java.lang.Double value) throws IllegalArgumentException {
            super(value, java.lang.Double.class);
        }
    }

    /**
     * Convenient class for Integer value
     * 
     * @author gprince
     * 
     */
    final class Integer extends Number<java.lang.Integer> {

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
        public Integer(java.lang.Integer value) throws IllegalArgumentException {
            super(value, java.lang.Integer.class);
        }
    }

    /**
     * Convenient class for Long value
     * 
     * @author gprince
     * 
     */
    final class Long extends Number<java.lang.Long> {

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
        public Long(java.lang.Long value) throws IllegalArgumentException {
            super(value, java.lang.Long.class);
        }
    }

    /**
     * This class represents an immutable Member of Json Object
     * 
     * @author gprince
     */
    final class Member implements Serializable {

        /**
         * Generated serialVersionUID
         */
        private static final long serialVersionUID = -5701026677118947699L;

        /**
         * Default constructor
         * 
         * @param name
         *            the name of this member
         * @param value
         *            the value of this member
         */
        public Member(java.lang.String name, JsonValue<?> value) {
            this.name = name;
            this.value = value;
        }

        /**
         * Returns the name of this member
         * 
         * @return the name of this member
         */
        java.lang.String getName() {
            return name;
        }

        /**
         * Returns the value of this member
         * 
         * @return the value of this member
         */
        JsonValue<?> getValue() {
            return value;
        }

        /**
         * Returns a string representation of this member
         */
        @Override
        public java.lang.String toString() {
            ToStringBuilder builder = new ToStringBuilder(this, new MemberToStringStyle()).append(name).append(value);
            return builder.toString();
        }

        /**
         * Immutable name of this member
         */
        private final java.lang.String name;

        /**
         * Immutable value of this member
         */
        private final JsonValue<?> value;

        /**
         * To String Style for Object members
         * 
         * @author gprince
         * 
         */
        private static final class MemberToStringStyle extends ToStringStyle {

            /**
             * Generated serialVersionUID
             */
            private static final long serialVersionUID = 7410446559015896437L;

            /**
             * Default Constructor
             */
            MemberToStringStyle() {
                super();
                this.setUseFieldNames(false);
                this.setUseClassName(false);
                this.setUseIdentityHashCode(false);
                this.setContentStart("");
                this.setFieldSeparator(": ");
                this.setContentEnd("");
            }
        }
    }

    /**
     * This class represents an immutable Json Object
     * 
     * @author gprince
     * 
     */
    final class Object extends AbstractValue<ArrayList<Member>> implements JsonStructure<Member> {

        /**
         * Generated serialVersionUID
         */
        private static final long serialVersionUID = -5452807402817850441L;

        /**
         * Default constructor
         * 
         * @param value
         *            The held value
         * @throws IllegalArgumentException
         *             if the value parameter is null
         */
        public Object(java.lang.String name) {
            super(new ArrayList<Member>());
            this.name = name;
        }

        /**
         * Appends the specified members to the end of this Object.
         * 
         * @parama name the member name
         * @param value
         *            the member value
         */
        public void add(java.lang.String name, JsonValue<?> value) {
            this.value.add(new Member(name, value));
        }

        @Override
        public void add(Object object) {
            this.value.add(new Member(object.getName(), object));
        }

        @Override
        public void add(Array array) {
            
        }

        /**
         * Returns an iterator over the members in this Object (in proper
         * sequence).
         */
        @Override
        public Iterator<Member> iterator() {
            return this.value.iterator();
        }

        /**
         * Returns the name of this object
         * 
         * @return The name of this object
         */
        public java.lang.String getName() {
            return name;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public ValueKind getValueKind() {
            return ValueKind.OBJECT;
        }

        @Override
        public java.lang.String toString() {
            ToStringBuilder builder = new ToStringBuilder(this, new ObjectToStringStyle());
            for (Member member : value) {
                builder.append(member);
            }
            return builder.toString();
        }

        /**
         * The object name
         */
        private java.lang.String name;

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
    }

    /**
     * This class represents an immutable Json String value
     * 
     * @author gprince
     * 
     */
    final class String extends AbstractValue<java.lang.String> {

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
        public String(java.lang.String value) {
            super(value, java.lang.String.class);
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
        public java.lang.String toString() {
            return STRING_DELIMITER.concat(super.toString()).concat(STRING_DELIMITER);
        }

        /**
         * Json string delimiter
         */
        private static final java.lang.String STRING_DELIMITER = "\"";

    }

    /**
     * Returns the kind of this JsonValue
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
     * @return
     */
    Class<? extends T> getType();
}