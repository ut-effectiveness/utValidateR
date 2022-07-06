         /* Course Validation Query */
         SELECT DISTINCT
                      a.term_id,
                      a.course_number,
                      a.section_number,
                      a.subject_code,
                      b.instructor_employee_id,
                      c.meet_building_id AS meet_building_id_1,
                      d.meet_building_id AS meet_building_id_2,
                      e.meet_building_id AS meet_building_id_3,
                      c.building_number AS building_number_1,
                      d.building_number AS building_number_2,
                      e.building_number AS building_number_3,
                      a.budget_code,
                      a.class_size,
                      a.college_id,
                      a.contact_hours,
                      c.meet_days AS meet_days_1,
                      d.meet_days AS meet_days_2,
                      e.meet_days AS meet_days_3,
                      a.instruction_method_code,
                      a.academic_department_id,
                      c.meet_end_date,
                      g.first_name || ' ' || g.last_name AS instructor_name,
                      a.section_format_type_code,
                      COALESCE(f.course_max_credits, f.course_min_credits) AS course_max_credits,
                      f.course_min_credits,
                      f.program_type,
                      c.meet_room_number AS meet_room_number_1,
                      d.meet_room_number AS meet_room_number_2,
                      e.meet_room_number AS meet_room_number_3,
                      c.room_max_occupancy AS room_max_occupancy_1,
                      d.room_max_occupancy AS room_max_occupancy_2,
                      e.room_max_occupancy AS room_max_occupancy_3,
                      c.room_use_code AS room_use_code_1,
                      d.room_use_code AS room_use_code_2,
                      e.room_use_code AS room_use_code_3,
                      a.campus_id,
                      c.meet_start_time AS meet_start_time_1,
                      d.meet_start_time AS meet_start_time_2,
                      e.meet_start_time AS meet_start_time_3,
                      c.meet_end_time AS meet_end_time_1,
                      d.meet_end_time AS meet_end_time_2,
                      e.meet_end_time AS meet_end_time_3,
                      f.course_title,
                      a.course_reference_number
                 FROM quad.student_section a
            LEFT JOIN quad.section_instructor_assignment b
                   ON b.section_id = a.section_id
                  AND b.is_primary_instructor
            LEFT JOIN quad.section_schedule c
                   ON c.section_id = a.section_id
                  AND c.building_room_rank = '1'
            LEFT JOIN quad.section_schedule d
                   ON d.section_id = a.section_id
                  AND d.building_room_rank = '2'
            LEFT JOIN quad.section_schedule e
                   ON e.section_id = a.section_id
                  AND e.building_room_rank = '3'
            LEFT JOIN quad.course f
                   ON f.course_id = a.course_id
            LEFT JOIN quad.employee g
                   ON g.employee_id = b.instructor_employee_id
            LEFT JOIN quad.term h
                   ON h.term_id = a.term_id
                WHERE a.term_id >= (SELECT term_id FROM quad.term WHERE is_previous_term) -- Previous Term and forward
                  AND is_enrolled = TRUE -- We may want to exclude this for validating.  If so we will need to exclude transfer courses: is_transfer = FALSE
                  ORDER BY a.term_id, a.course_number;