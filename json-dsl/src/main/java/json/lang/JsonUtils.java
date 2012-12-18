package json.lang;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import json.lang.JsonValue.Array;
import json.lang.JsonValue.Member;
import json.lang.JsonValue.Object;

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

}