package json.lang;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import json.lang.JsonValue.JsonArray;
import json.lang.JsonValue.JsonObject;

/**
 * JSON Utils
 * 
 * @author gprince
 * 
 */
public class JsonUtils {

	/**
	 * @param object
	 * @return
	 */
	public static Map<String, java.lang.Object> unmodifiableMap(
			JsonObject object) {
		if (object == null)
			return null;

		if (isEmpty(object))
			return new LinkedHashMap<>();

		Map<String, java.lang.Object> map = new LinkedHashMap<>();
		for (String name : object) {
			JsonValue<?> value = object.get(name);
			switch (value.getValueKind()) {
			case ARRAY:
				map.put(name, unmodifiableList((JsonArray) value));
				break;

			case OBJECT:
				map.put(name, unmodifiableMap((JsonObject) value));
				break;

			default:
				map.put(name, value.getValue());
				break;
			}
		}
		return Collections.unmodifiableMap(map);
	}

	/**
	 * @param array
	 * @return
	 */
	public static List<java.lang.Object> unmodifiableList(JsonArray array) {
		if (array == null)
			return null;

		if (isEmpty(array))
			return new ArrayList<>();

		List<java.lang.Object> list = new ArrayList<>();
		for (JsonValue<?> value : array.getValue()) {
			switch (value.getValueKind()) {
			case ARRAY:
				list.add(unmodifiableList((JsonArray) value));
				break;

			case OBJECT:
				list.add(unmodifiableMap((JsonObject) value));
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
	public static boolean isEmpty(JsonArray array) {
		if (array == null)
			return true;

		return array.getValue().isEmpty();
	}

	/**
	 * @param array
	 * @return
	 */
	public static boolean isNotEmpty(JsonArray array) {
		return !isEmpty(array);
	}

	/**
	 * @param object
	 * @return
	 */
	public static boolean isEmpty(JsonObject object) {
		if (object == null)
			return true;

		return object.getValue().isEmpty();
	}

	/**
	 * @param object
	 * @return
	 */
	public static boolean isNotEmpty(JsonObject object) {
		return !isEmpty(object);
	}

}