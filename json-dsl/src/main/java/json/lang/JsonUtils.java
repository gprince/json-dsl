package json.lang;

import static json.lang.JsonValue.NULL_OBJECT;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import json.lang.JsonValue.Array;
import json.lang.JsonValue.Member;
import json.lang.JsonValue.Object;

import org.apache.commons.collections.CollectionUtils;

/**
 * @author gprince
 * 
 */
public class JsonUtils {

	/**
	 * @param object
	 * @return
	 */
	public static Map<String, java.lang.Object> unmodifiableMap(Object object) {
		if (object == null)
			return null;

		if (isEmpty(object))
			return new LinkedHashMap<>();

		Map<String, java.lang.Object> map = new LinkedHashMap<>();
		for (Member member : object.getValue()) {
			JsonValue<?> value = member.getValue();
			switch (value.getValueKind()) {
			case ARRAY:
				map.put(member.getName(), unmodifiableList((Array) value));
				break;

			case OBJECT:
				map.put(member.getName(), unmodifiableMap((Object) value));
				break;

			default:
				map.put(member.getName(), value.getValue());
				break;
			}
		}
		return Collections.unmodifiableMap(map);
	}

	/**
	 * @param array
	 * @return
	 */
	public static List<java.lang.Object> unmodifiableList(Array array) {
		if (array == null)
			return null;

		if (isEmpty(array))
			return new ArrayList<>();

		List<java.lang.Object> list = new ArrayList<>();
		for (JsonValue<?> value : array.getValue()) {
			switch (value.getValueKind()) {
			case ARRAY:
				list.add(unmodifiableList((Array) value));
				break;

			case OBJECT:
				list.add(unmodifiableMap((Object) value));
				break;

			default:
				list.add(value.getValue());
				break;
			}
		}

		return Collections.unmodifiableList(list);
	}

	/**
	 * @param array
	 * @return
	 */
	public static boolean isEmpty(Array array) {
		if (array == null)
			return true;

		return array.getValue().isEmpty();
	}

	/**
	 * @param object
	 * @return
	 */
	public static boolean isEmpty(Object object) {
		if (object == null)
			return true;

		return object.getValue().isEmpty();
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public static <T> Array array(Collection<T> collection) {

		if (CollectionUtils.isEmpty(collection)) {
			return null;
		}

		Array array = new Array();
		for (T object : collection) {

			if (object == null) {
				array.add(NULL_OBJECT);
			}

			else if (BigDecimal.class.isInstance(object)) {
				array.add(new json.lang.JsonValue.BigDecimal(
						(BigDecimal) object));
			}

			else if (Boolean.class.isInstance(object)
					|| Boolean.TYPE.isInstance(object)) {
				Boolean bool = (Boolean) object;
				if (bool)
					array.add(json.lang.JsonValue.TRUE);
				else
					array.add(json.lang.JsonValue.FALSE);
			}

			else if (Integer.class.isInstance(object) || Integer.TYPE.isInstance(object)) {
				array.add(new json.lang.JsonValue.Integer((Integer) object));
			}

			else if (Long.class.isInstance(object) || Long.TYPE.isInstance(object)) {
				array.add(new json.lang.JsonValue.Long((Long) object));
			}
			
			else if (Number.class.isInstance(object)) {
				array.add(new json.lang.JsonValue.Long((Long) object));
			}

			else if (String.class.isInstance(object)) {
				array.add(new json.lang.JsonValue.String((String) object));
			}
			
			else if (Collection.class.isInstance(object)) {
				array.add(array((Collection) object));
			}
		}

		return array;
	}

	public static void main(String[] args) {
		BigDecimal bigd = new BigDecimal(8977.584);
		System.out.println(BigDecimal.class.isInstance(bigd));

		bigd = null;
		System.out.println(BigDecimal.class.isInstance(bigd));

	}
}