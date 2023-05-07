CREATE INDEX schedules_user_id_index ON schedules (user_id);
ALTER TABLE schedules ADD CONSTRAINT schedules_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE NO ACTION;
