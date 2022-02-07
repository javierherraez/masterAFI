-- 1. Obtén un listado de todos los circuitos que se encuentren en España.
SELECT c.Ref FROM circuit c WHERE country = 'Spain';

-- 2. Obtén todos los pilotos que formaron parte de la escudería Ferrari en la temporada 2009
-- y ordénalos alfabéticamente de manera inversa (los nombres que empiezan por la letra
-- Z deberán aparecer en primer lugar).

SELECT d.Ref, d.name, d.surname FROM driver d
JOIN driver_drives_for_constructor dc ON d.Id = dc.driver_id
JOIN constructor c ON dc.constructor_id = c.Id
WHERE dc.season = 2009 AND c.name = 'Ferrari'
ORDER BY d.name DESC;

--3. ¿Cuál es el nombre de la última carrera celebrada?
SELECT TOP 1 r.name, r.date FROM race r
ORDER BY r.date DESC;

--4. Obtén la posición media del piloto Lewis Hamilton para todas las carreras en las que
--participó.
SELECT AVG(CAST(r.positionOrder AS FLOAT)) posicion_media FROM result r
JOIN driver d ON r.driver_id = d.Id
WHERE d.name = 'Lewis' AND d.surname = 'Hamilton';

--5. ¿Qué piloto ha participado en el mayor número de carreras?
SELECT TOP 1 d.Ref, d.name, d.surname, COUNT(*) n_carreras FROM result r
JOIN driver d ON d.Id = r.driver_id
GROUP BY d.Ref, d.name, d.surname
ORDER BY COUNT(*) DESC;

--6. ¿Qué pilotos han participado en al menos 20 temporadas?
SELECT d.Ref, d.name, d.surname, COUNT(DISTINCT dc.season) n_temporadas FROM driver d
JOIN driver_drives_for_constructor dc ON d.Id = dc.driver_id
GROUP BY d.Ref, d.name, d.surname
-- HAVING COUNT(DISTINCT season) >= 20 condición comentada para ver si existían resultados
ORDER BY n_temporadas DESC;

--7. Obtén un listado de los pilotos cuyo nombre empiece por ‘Joe’ y ordena sus apellidos
--de forma descendente.
SELECT d.Ref, d.name, d.surname FROM driver d
WHERE d.name LIKE 'Joe%'
ORDER BY d.surname DESC;

--8. Calcula los puntos totales que obtuvo el piloto Fernando Alonso en la temporada 2005.
SELECT SUM(rs.points) puntos FROM driver d
JOIN result rs ON d.Id = rs.driver_id
JOIN race ra ON rs.race_id = ra.Id
WHERE d.name = 'Fernando' AND d.surname = 'Alonso' AND ra.season = 2005;

--9. Obtén un listado de todos los pilotos que han formado parte de la escudería Renault.
SELECT DISTINCT d.Ref, d.name, d.surname FROM driver d
JOIN driver_drives_for_constructor dc ON d.Id = dc.driver_id
JOIN constructor c ON dc.constructor_id = c.Id
WHERE c.name = 'Renault'

--10. Calcula la diferencia de puntos obtenidos en la temporada 2010 entre los pilotos
--Fernando Alonso y Lewis Hamilton.

-- (Aunque no hemos aprendido la expresión CASE en clase, estaba familiarizado con ella y utilizarla me parecía la opción más sencilla)
SELECT 
	SUM(CASE WHEN d.name = 'Fernando' AND d.surname = 'Alonso' AND ra.season = 2010 THEN rs.points END) puntos_alonso,
	SUM(CASE WHEN d.name = 'Lewis' AND d.surname = 'Hamilton' AND ra.season = 2010 THEN rs.points END) puntos_hamilton,
	SUM(CASE WHEN d.name = 'Fernando' AND d.surname = 'Alonso' AND ra.season = 2010 THEN rs.points END)
		- SUM(CASE WHEN d.name = 'Lewis' AND d.surname = 'Hamilton' AND ra.season = 2010 THEN rs.points END) [diferencia puntos]
FROM driver d
JOIN result rs ON d.Id = rs.driver_id
JOIN race ra ON rs.race_id = ra.Id;