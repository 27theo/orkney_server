let generate_uid prefix = prefix ^ Dream.to_base64url (Dream.random 8)
