package json.io;

import static json.lang.JsonValue.NULL;

import java.io.IOException;
import java.io.Writer;

import json.lang.JsonValue;
import json.lang.JsonValue.JsonArray;
import json.lang.JsonValue.JsonObject;

/**
 * @author Gregory
 * 
 */
public class JsonWriter {

    /**
     * @param out
     */
    public JsonWriter(Writer out) {
        this.out = out;
    }

    public void write(JsonArray array) throws IOException {
        startArray();
        for (JsonValue<?> value : array.getValue()) {
            write(value);
        }
        endArray();
    }

    public void write(JsonObject object) throws IOException {
        startObject();
        boolean first = true;
        for (String name : object) {
            if (!first) {
                out.append(MEMBER_SEPARATOR);
            }

            else {
                first = false;
            }
            startString();
            out.append(name);
            endString();
            out.append(VALUE_SEPARATOR);
            write(object.get(name));
        }
        endObject();
    }

    private void write(JsonValue<?> value) throws IOException {
        switch (value.getValueKind()) {
        case ARRAY:
            write((JsonArray) value);
            break;

        case OBJECT:
            write((JsonObject) value);
            break;

        case NUMBER:
            out.append(((Number) value.getValue()).toString());
            break;

        case STRING:
            startString();
            out.append((String) value.getValue());
            endString();
            break;

        case TRUE:
            out.append(Boolean.TRUE.toString());
            break;

        case FALSE:
            out.append(Boolean.FALSE.toString());
            break;

        case NULL:
            out.append(NULL);
            break;
        }

    }

    private void startArray() throws IOException {
        out.append('[');
    }

    private void endArray() throws IOException {
        out.append(']');
    }

    private void startString() throws IOException {
        out.append('"');
    }

    private void endString() throws IOException {
        out.append('"');
    }

    private void startObject() throws IOException {
        out.append('{');
    }

    private void endObject() throws IOException {
        out.append('}');
    }

    /**
     * 
     */
    private static final String MEMBER_SEPARATOR = ",";

    /**
     * 
     */
    private static final String VALUE_SEPARATOR = ":";

    /**
     * Le writer
     */
    private Writer out;
}