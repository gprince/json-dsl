package json.builder;

import java.math.BigDecimal;

import json.lang.JsonValue.JsonArray;
import json.lang.JsonValue.JsonObject;
import json.lang.exception.JsonBuildException;

/**
 * @author gprince
 * 
 * @param <T>
 *            Type de builder retourné à la fin de la construction d'une
 *            JsonStructure
 */
public interface JsonArrayAppender<T extends JsonBuilder> {

	/**
	 * Commence un nouveau JsonArray à rajouter au JsonArray en cours de
	 * construction
	 * 
	 * @return un nouveau builder pour le nouveau JsonArray à construire et à
	 *         rajouter
	 * @throws JsonBuildException
	 */
	JsonArrayBuilder<T> startArray();

	/**
	 * Commence un nouveau JsonObject à rajouter au JsonArray en cours de
	 * construction
	 * 
	 * @return un nouveau builder pour le nouveau JsonObject à construire et à
	 *         rajouter
	 */
	JsonObjectBuilder<T> startObject();

	/**
	 * Ajoute un {@link BigDecimal} comme valeur du JsonArray à construire
	 * 
	 * @param value
	 *            valeur du membre
	 * @return le builder courant afin de pouvoir chaîner les appels
	 * @throws JsonBuildException
	 *             si le nom est null
	 */
	T add(BigDecimal value);

	/**
	 * Ajoute un {@link Boolean} comme valeur du JsonArray à construire
	 * 
	 * @param value
	 *            valeur du membre
	 * @return le builder courant afin de pouvoir chaîner les appels
	 * @throws JsonBuildException
	 *             si le nom est null
	 */
	T add(Boolean value);

	/**
	 * Ajoute un {@link Double} comme valeur du JsonArray à construire
	 * 
	 * @param value
	 *            valeur du membre
	 * @return le builder courant afin de pouvoir chaîner les appels
	 * @throws JsonBuildException
	 *             si le nom est null
	 */
	T add(Double value);

	/**
	 * Ajoute un {@link Integer} comme valeur du JsonArray à construire
	 * 
	 * @param value
	 *            valeur du membre
	 * @return le builder courant afin de pouvoir chaîner les appels
	 * @throws JsonBuildException
	 *             si le nom est null
	 */
	T add(Integer value);

	/**
	 * Ajoute un {@link Long} comme valeur du JsonArray à construire
	 * 
	 * @param value
	 *            valeur du membre
	 * @return le builder courant afin de pouvoir chaîner les appels
	 * @throws JsonBuildException
	 *             si le nom est null
	 */
	T add(Long value);

	/**
	 * Ajoute un {@link String} comme valeur du JsonArray à construire
	 * 
	 * @param value
	 *            valeur du membre
	 * @return le builder courant afin de pouvoir chaîner les appels
	 * @throws JsonBuildException
	 *             si le nom est null
	 */
	T add(String value);

	/**
	 * Ajoute un {@link JsonObject} comme valeur du JsonArray à construire
	 * 
	 * @param value
	 *            valeur du membre
	 * @return le builder courant afin de pouvoir chaîner les appels
	 * @throws JsonBuildException
	 *             si le nom est null
	 */
	T add(JsonObject object);

	/**
	 * Ajoute un {@link JsonArray} comme valeur du JsonArray à construire
	 * 
	 * @param value
	 *            valeur du membre
	 * @return le builder courant afin de pouvoir chaîner les appels
	 * @throws JsonBuildException
	 *             si le nom est null
	 */
	T add(JsonArray array);

	/**
	 * Merge le JsonArray passé en paramètre avec le JsonArray en cours de
	 * construction
	 * 
	 * <p>
	 * Toutes les valeurs du JsonArray à merger sont rajoutés comme valeurs du
	 * JsonArray en cours de construction
	 * </p>
	 * 
	 * @param object
	 *            le JsonObject à merger
	 * @return
	 * @throws JsonBuildException
	 *             si le nom est null
	 */
	T merge(JsonArray array);
}