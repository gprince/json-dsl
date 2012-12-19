package json.builder;

import static json.lang.JsonValue.FALSE;
import static json.lang.JsonValue.NULL_BIG_DECIMAL;
import static json.lang.JsonValue.NULL_BOOLEAN;
import static json.lang.JsonValue.NULL_DOUBLE;
import static json.lang.JsonValue.NULL_INTEGER;
import static json.lang.JsonValue.NULL_LONG;
import static json.lang.JsonValue.NULL_STRING;
import static json.lang.JsonValue.NULL_COLLECTION;
import static json.lang.JsonValue.TRUE;

import java.util.Collection;

import json.lang.JsonUtils;
import json.lang.JsonValue;
import json.lang.JsonValue.Null;
import json.lang.JsonValue.Array;
import json.lang.JsonValue.BigDecimal;
import json.lang.JsonValue.Double;
import json.lang.JsonValue.Integer;
import json.lang.JsonValue.JsonStructure;
import json.lang.JsonValue.Long;
import json.lang.JsonValue.Object;
import json.lang.JsonValue.String;
import json.lang.exception.JsonBuildException;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;

/**
 * @author gprince
 * 
 */
public final class Builders {

	// ---------- ---------- ----------
	// PUBLIC
	// ---------- ---------- ----------

	public static RootObjectBuilder jsonObject() {
		return new RootObjectBuilder();
	}

	public static RootArrayBuilder jsonArray() {
		return new RootArrayBuilder();
	}

	protected interface Buildable<V extends JsonStructure<?>> {

		V build();

	}

	/**
	 * @author gprince
	 * 
	 * @param <B>
	 */
	protected interface IObjectBuilder<B extends IObjectBuilder<?>> {

		B add(java.lang.String name, java.math.BigDecimal value)
				throws JsonBuildException;

		B add(java.lang.String name, java.lang.Boolean value)
				throws JsonBuildException;

		B add(java.lang.String name, java.lang.Double value)
				throws JsonBuildException;

		B add(java.lang.String name, java.lang.Integer value)
				throws JsonBuildException;

		B add(java.lang.String name, java.lang.Long value)
				throws JsonBuildException;

		B add(java.lang.String name, java.lang.String value)
				throws JsonBuildException;

		<T> B add(java.lang.String name, Collection<T> value)
				throws JsonBuildException;

	}

	/**
	 * @author gprince
	 * 
	 * @param <B>
	 */
	protected interface IArrayBuilder<B extends IArrayBuilder<?>> {

		B add(java.math.BigDecimal value);

		B add(java.lang.Boolean value);

		B add(java.lang.Double value);

		B add(java.lang.Integer value);

		B add(java.lang.Long value);

		B add(java.lang.String value);

	}

	private static abstract class Builder<V extends JsonStructure<?>> {

		// ---------- ---------- ----------
		// PUBLIC
		// ---------- ---------- ----------

		protected java.lang.String getName() {
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

		private Builder(java.lang.String name, V value) {
			this.name = name;
			this.value = value;
		}

		private static final java.lang.String ANONYMOUS = "[anonymous]";

		private final java.lang.String name;

		private final V value;
	}

	public static abstract class RootBuilder<V extends JsonStructure<?>>
			extends Builder<V> implements Buildable<V> {

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

	public static class RootObjectBuilder extends RootBuilder<Object> implements
			IObjectBuilder<RootObjectBuilder> {

		@Override
		public RootObjectBuilder add(java.lang.String name,
				java.math.BigDecimal value) throws JsonBuildException {
			// Name can not be null
			if (StringUtils.isEmpty(name)) {
				throw new JsonBuildException(BUILD_EXCEPTION_NULL_NAME,
						new IllegalArgumentException());
			}

			if (value != null) {
				getValue().add(name, new BigDecimal(value));
			}

			else {
				getValue().add(name, NULL_BIG_DECIMAL);
			}
			return this;
		}

		@Override
		public RootObjectBuilder add(java.lang.String name,
				java.lang.Boolean value) throws JsonBuildException {
			// Name can not be null
			if (StringUtils.isEmpty(name)) {
				throw new JsonBuildException(BUILD_EXCEPTION_NULL_NAME,
						new IllegalArgumentException());
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
		public RootObjectBuilder add(java.lang.String name,
				java.lang.Double value) throws JsonBuildException {
			// Name can not be null
			if (StringUtils.isEmpty(name)) {
				throw new JsonBuildException(BUILD_EXCEPTION_NULL_NAME,
						new IllegalArgumentException());
			}

			if (value != null) {
				getValue().add(name, new Double(value));
			}

			else {
				getValue().add(name, NULL_DOUBLE);
			}
			return this;
		}

		@Override
		public RootObjectBuilder add(java.lang.String name,
				java.lang.Integer value) throws JsonBuildException {
			// Name can not be null
			if (StringUtils.isEmpty(name)) {
				throw new JsonBuildException(BUILD_EXCEPTION_NULL_NAME,
						new IllegalArgumentException());
			}

			if (value != null) {
				getValue().add(name, new Integer(value));
			}

			else {
				getValue().add(name, NULL_INTEGER);
			}
			return this;
		}

		@Override
		public RootObjectBuilder add(java.lang.String name, java.lang.Long value)
				throws JsonBuildException {
			// Name can not be null
			if (StringUtils.isEmpty(name)) {
				throw new JsonBuildException(BUILD_EXCEPTION_NULL_NAME,
						new IllegalArgumentException());
			}

			if (value != null) {
				getValue().add(name, new Long(value));
			}

			else {
				getValue().add(name, NULL_LONG);
			}
			return this;
		}

		@Override
		public RootObjectBuilder add(java.lang.String name,
				java.lang.String value) throws JsonBuildException {
			// Name can not be null
			if (StringUtils.isEmpty(name)) {
				throw new JsonBuildException(BUILD_EXCEPTION_NULL_NAME,
						new IllegalArgumentException());
			}

			if (value != null) {
				getValue().add(name, new String(value));
			}

			else {
				getValue().add(name, NULL_STRING);
			}
			return this;
		}

		@SuppressWarnings("unchecked")
		@Override
		public <T> RootObjectBuilder add(java.lang.String name,
				Collection<T> collection) throws JsonBuildException {
			// Name can not be null
			if (StringUtils.isEmpty(name)) {
				throw new JsonBuildException(BUILD_EXCEPTION_NULL_NAME,
						new IllegalArgumentException());
			}

			if (CollectionUtils.isEmpty(collection)) {
				getValue().add(name, NULL_COLLECTION);
			}

			else {
				getValue().add(name, JsonUtils.array(collection));
			}

			return this;
		}

		// ---------- ---------- ----------
		// PRIVATE
		// ---------- ---------- ----------

		private static final java.lang.String BUILD_EXCEPTION_NULL_NAME = "Name can not be null!";

		private RootObjectBuilder() {
			super(new Object());
		}

	}

	public static class RootArrayBuilder extends RootBuilder<Array> implements
			IArrayBuilder<RootArrayBuilder> {

		// ---------- ---------- ----------
		// PUBLIC
		// ---------- ---------- ----------

		@Override
		public RootArrayBuilder add(java.math.BigDecimal value) {
			if (value != null) {
				getValue().add(new BigDecimal(value));
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
		public RootArrayBuilder add(java.lang.Double value) {
			if (value != null) {
				getValue().add(new Double(value));
			} else {
				getValue().add(NULL_DOUBLE);
			}
			return this;
		}

		@Override
		public RootArrayBuilder add(java.lang.Integer value) {
			if (value != null) {
				getValue().add(new Integer(value));
			} else {
				getValue().add(NULL_INTEGER);
			}
			return this;
		}

		@Override
		public RootArrayBuilder add(java.lang.Long value) {
			if (value != null) {
				getValue().add(new Long(value));
			} else {
				getValue().add(NULL_LONG);
			}
			return this;
		}

		@Override
		public RootArrayBuilder add(java.lang.String value) {
			if (value != null) {
				getValue().add(new String(value));
			} else {
				getValue().add(NULL_STRING);
			}
			return this;
		}

		// ---------- ---------- ----------
		// PRIVATE
		// ---------- ---------- ----------

		private RootArrayBuilder() {
			super(new Array());
		}

	}

	protected static abstract class ChildBuilder<T extends Builder<?>, V extends JsonStructure<?>>
			extends Builder<V> {

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
		private static final java.lang.String BUILD_EXCEPTION = "Values other than Arrays and Objects can not be build!";

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
				Array array = (Array) value;
				array.add((JsonValue) getValue());
				break;

			case OBJECT:
				Object object = (Object) value;
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

		private ChildBuilder(T parentBuilder, java.lang.String name, V value) {
			super(name, value);
			this.parentBuilder = parentBuilder;
		}

		/**
		 * The parent builder
		 */
		private final T parentBuilder;
	}

	public static class ObjectBuilder<T extends Builder<?>> extends
			ChildBuilder<T, Object> implements IObjectBuilder<ObjectBuilder<T>> {

		// ---------- ---------- ----------
		// PUBLIC
		// ---------- ---------- ----------

		public ArrayBuilder<ObjectBuilder<T>> startArray(java.lang.String name) {
			return new ArrayBuilder<ObjectBuilder<T>>(this, name);
		}

		public ObjectBuilder<ObjectBuilder<T>> startObject(java.lang.String name) {
			return new ObjectBuilder<ObjectBuilder<T>>(this, name);
		}

		@Override
		public ObjectBuilder<T> add(java.lang.String name,
				java.math.BigDecimal value) {
			if (value != null) {
				getValue().add(name, new BigDecimal(value));
			} else {
				getValue().add(name, NULL_BIG_DECIMAL);
			}
			return this;
		}

		@Override
		public ObjectBuilder<T> add(java.lang.String name,
				java.lang.Boolean value) {
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
		public ObjectBuilder<T> add(java.lang.String name,
				java.lang.Double value) {
			if (value != null) {
				getValue().add(name, new Double(value));
			} else {
				getValue().add(name, NULL_DOUBLE);
			}
			return this;
		}

		@Override
		public ObjectBuilder<T> add(java.lang.String name,
				java.lang.Integer value) {
			if (value != null) {
				getValue().add(name, new Integer(value));
			} else {
				getValue().add(name, NULL_INTEGER);
			}
			return this;
		}

		@Override
		public ObjectBuilder<T> add(java.lang.String name, java.lang.Long value) {
			if (value != null) {
				getValue().add(name, new Long(value));
			} else {
				getValue().add(name, NULL_LONG);
			}
			return this;
		}

		@Override
		public ObjectBuilder<T> add(java.lang.String name,
				java.lang.String value) {
			if (value != null) {
				getValue().add(name, new String(value));
			} else {
				getValue().add(name, NULL_STRING);
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

		private ObjectBuilder(T parentBuilder) {
			super(parentBuilder, new Object());
		}

		private ObjectBuilder(T parentBuilder, java.lang.String name) {
			super(parentBuilder, name, new Object());
		}

		@Override
		public <C> ObjectBuilder<T> add(java.lang.String name,
				Collection<C> collection) throws JsonBuildException {
			if (CollectionUtils.isEmpty(collection)) {
				getValue().add(name, NULL_COLLECTION);
			}

			else {
				getValue().add(name, JsonUtils.array(collection));
			}
			return this;
		}
	}

	public static class ArrayBuilder<T extends Builder<?>> extends
			ChildBuilder<T, Array> implements IArrayBuilder<ArrayBuilder<T>> {

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
		public ArrayBuilder<T> add(java.math.BigDecimal value) {
			if (value != null) {
				getValue().add(new BigDecimal(value));
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
		public ArrayBuilder<T> add(java.lang.Double value) {
			if (value != null) {
				getValue().add(new Double(value));
			} else {
				getValue().add(NULL_DOUBLE);
			}
			return this;
		}

		@Override
		public ArrayBuilder<T> add(java.lang.Integer value) {
			if (value != null) {
				getValue().add(new Integer(value));
			} else {
				getValue().add(NULL_INTEGER);
			}
			return this;
		}

		@Override
		public ArrayBuilder<T> add(java.lang.Long value) {
			if (value != null) {
				getValue().add(new Long(value));
			} else {
				getValue().add(NULL_LONG);
			}
			return this;
		}

		@Override
		public ArrayBuilder<T> add(java.lang.String value) {
			if (value != null) {
				getValue().add(new String(value));
			} else {
				getValue().add(NULL_STRING);
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

		private ArrayBuilder(T parentBuilder) {
			super(parentBuilder, new Array());
		}

		private ArrayBuilder(T parentBuilder, java.lang.String name) {
			super(parentBuilder, name, new Array());
		}
	}
}