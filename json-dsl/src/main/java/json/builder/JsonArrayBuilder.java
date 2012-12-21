package json.builder;

import json.lang.exception.JsonBuildException;

/**
 * Contrat pour un builder permettant de construire un
 * {@link json.lang.JsonValue.JsonArray}
 * 
 * @author gprince
 * 
 * @param <T>
 *            Type de builder retourné à la fin de la construction d'une
 *            JsonStructure
 */
public interface JsonArrayBuilder<T extends JsonBuilder> extends
		JsonArrayAppender<JsonArrayBuilder<T>>, JsonBuilder {

	/**
	 * Construit le JsonArray en cours de construction, rajoute le au builder
	 * parent et retourne ce dernier
	 * 
	 * @return le JsonBuilder parent
	 * @throws JsonBuildException
	 */
	T endArray() throws JsonBuildException;

}