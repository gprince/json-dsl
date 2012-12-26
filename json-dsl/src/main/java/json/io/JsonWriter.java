package json.io;

import static json.lang.JsonValue.NULL;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import json.lang.JsonValue;
import json.lang.JsonValue.JsonArray;
import json.lang.JsonValue.JsonObject;

/**
 * @author Gregory
 * 
 */
/**
 * @author gprince
 *
 */
public class JsonWriter {

	/**
	 * Caractéristique de configuration d'un Json Writer
	 * 
	 * @author gprince
	 *
	 */
	public enum Feature {
		/**
		 * Indente le Json
		 */
		INDENT
	}

	/**
	 * Objet de configuration d'un Writer
	 * 
	 * @author gprince
	 *
	 */
	public static class Configuration implements Iterable<Feature> {

		/**
		 * Constructeur par défaut
		 */
		public Configuration() {
			features = new ArrayList<>();
		}

		/**
		 * Retoure un {@link Iterator} sur la liste des {@link Feature} de cette configuraion
		 * 
		 * @see java.lang.Iterable#iterator()
		 */
		@Override
		public Iterator<Feature> iterator() {
			return features.iterator();
		}

		/**
		 * Ajoute une Feature
		 * 
		 * @param feature
		 */
		public void add(Feature feature) {
			this.features.add(feature);
		}

		/**
		 * Liste de Feature
		 */
		private List<Feature> features;

	}

	/**
	 * Consructeur par défaut
	 * 
	 * @param out le writer de base
	 */
	public JsonWriter(Writer out) {
		this.out = out;
	}

	/**
	 * Consructeur par défaut
	 * 
	 * @param out le writer de base
	 */
	public JsonWriter(Writer out, Configuration configuration) {
		this.out = out;
		this.configuration = configuration;
	}

	/**
	 * Ecrit un Json Array
	 * 
	 * @param array
	 * @throws IOException
	 */
	public void write(JsonArray array) throws IOException {
		startArray();
		for (JsonValue<?> value : array.getValue()) {
			write(value);
		}
		endArray();
	}

	/**
	 * Ecrit un Json Object
	 * 
	 * @param object
	 * @throws IOException
	 */
	public void write(JsonObject object) throws IOException {
		startObject();
		boolean first = true;
		for (String name : object) {
			if (!first) {
				out.append(MEMBER_SEPARATOR);
			} else {
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

	/**
	 * Ecrit une Json Value
	 * s
	 * @param value
	 * @throws IOException
	 */
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

	/**
	 * Débute un Json Array
	 * 
	 * @throws IOException
	 */
	private void startArray() throws IOException {
		out.append('[');
	}

	/**
	 * Fini un Json Array
	 * 
	 * @throws IOException
	 */
	private void endArray() throws IOException {
		out.append(']');
	}

	/**
	 * @throws IOException
	 */
	private void startString() throws IOException {
		out.append('"');
	}

	/**
	 * @throws IOException
	 */
	private void endString() throws IOException {
		out.append('"');
	}

	/**
	 * @throws IOException
	 */
	private void startObject() throws IOException {
		out.append('{');
	}

	/**
	 * @throws IOException
	 */
	private void endObject() throws IOException {
		out.append('}');
	}

	/**
	 * Nombre de caractère d'indentation par défaut
	 */
	private static final int DEFAULT_INDENTATION = 4;

	/**
	 * Caractère d'indentation par défaut : ESPACE
	 */
	private static final char DEFAULT_INDENTATION_CHARACTER = ' ';

	/**
	 * Séparateur de membre
	 */
	private static final String MEMBER_SEPARATOR = ",";

	/**
	 * Séparateur de paire name / value d'un membre
	 */
	private static final String VALUE_SEPARATOR = ":";

	/**
	 * Indentation par défaut
	 */
	private int indentation = DEFAULT_INDENTATION;

	/**
	 * Caractère d'indentation
	 */
	private char indentCharacter = DEFAULT_INDENTATION_CHARACTER;

	/**
	 * Le writer
	 */
	private Writer out;

	/**
	 * La configuration du writer
	 */
	private Configuration configuration;
}