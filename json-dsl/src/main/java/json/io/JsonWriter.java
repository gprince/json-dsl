package json.io;

import static json.builder.Builders.jsonArray;
import static json.builder.Builders.jsonObject;
import static json.lang.JsonValue.NULL;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.math.BigDecimal;

import json.lang.JsonValue;
import json.lang.JsonValue.JsonArray;
import json.lang.JsonValue.JsonMember;
import json.lang.JsonValue.JsonObject;
import json.lang.exception.JsonBuildException;

public class JsonWriter {

    public static class Feature {

        enum Features {
            PRETTY_PRINT
        };

    }

    public JsonWriter(Writer out) {
        this.out = out;
    }

    public JsonWriter(Writer out, Feature feature) {
        this.out = out;
        this.feature = feature;
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
        for (JsonMember member : object.getValue()) {
            write(member);
        }
        endObject();
    }

    private void write(JsonMember member) throws IOException {
        startString();
        out.append(member.getName());
        endString();
        out.append(':');
        write(member.getValue());
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
            out.append((String) value.getValue());
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

    private Feature feature;

    private Writer out;

    public static void main(String[] args) {
        try {
            BigDecimal number = new BigDecimal(1890734.589);
            boolean vrai = true;
            String foo = "Foo";
            JsonObject object = jsonObject().add("number", number).add("vrai", vrai).add("foo", foo).startObject()
                    .add("long", -7894561230L).endObject().build();

            JsonWriter writer = new JsonWriter(new BufferedWriter(new OutputStreamWriter(System.out)));
            writer.write(object);

        } catch (JsonBuildException | IOException e) {
            e.printStackTrace(System.err);
        }
    }
}