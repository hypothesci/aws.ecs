
#' Title
#'
#' @param region
#' @param family
#' @param cluster
#' @param subnets
#' @param security_groups
#' @param assign_public_ip
#' @param revision
#' @param count
#' @param environment
#' @param command
#' @param started_by
#'
#' @return
#' @export
#'
#' @examples
run_task <- function(region, family, cluster, subnets, security_groups = list(), assign_public_ip = F,
	revision = NULL, count = 1, environment = list(), command = c(), started_by = "") {
	task_def <- if (!is.null(revision)) paste0(family, ":", revision) else family

	ecs_http(region, "POST", "RunTask", body = list(
		cluster = jsonlite::unbox(cluster),
		count = jsonlite::unbox(count),
		launchType = jsonlite::unbox("FARGATE"),
		taskDefinition = jsonlite::unbox(task_def),
		overrides = list(
			containerOverrides = list(list(
				name = jsonlite::unbox(family),
				environment = environment,
				command = command
			))
		),
		networkConfiguration = list(
			awsvpcConfiguration = list(
				assignPublicIp = jsonlite::unbox(ifelse(assign_public_ip, "ENABLED", "DISABLED")),
				securityGroups = security_groups,
				subnets = subnets
			)
		),
		startedBy = jsonlite::unbox(started_by)
	))
}

#' Title
#'
#' @param region
#' @param tasks
#' @param cluster
#'
#' @return
#' @export
#'
#' @examples
describe_tasks <- function(region, tasks, cluster) {
	ecs_http(region, "POST", "DescribeTasks", body = list(
		tasks = tasks,
		cluster = jsonlite::unbox(cluster)
	))
}

#' Title
#'
#' @param region
#' @param family
#' @param image
#' @param execution_role
#' @param task_role
#' @param cpu
#' @param memory
#' @param log_group
#' @param log_stream_prefix
#'
#' @return
#' @export
#'
#' @examples
register_task_definition <- function(region, family, image, execution_role, task_role, cpu, memory,
	log_group = family, log_stream_prefix = family) {
	ecs_http(region, "POST", "RegisterTaskDefinition", body = list(
		networkMode = jsonlite::unbox("awsvpc"),
		containerDefinitions = list(list(
			name = jsonlite::unbox(family),
			image = jsonlite::unbox(image),
			essential = jsonlite::unbox(T),
			cpu = jsonlite::unbox(cpu),
			memory = jsonlite::unbox(memory),
			logConfiguration = list(
				logDriver = jsonlite::unbox("awslogs"),
				options = list(
					"awslogs-group" = jsonlite::unbox(log_group),
					"awslogs-region" = jsonlite::unbox(region),
					"awslogs-stream-prefix" = jsonlite::unbox(log_stream_prefix)
				)
			)
		)),
		family = jsonlite::unbox(family),
		requiresCompatibilities = "FARGATE",
		compatibilities = "FARGATE",
		cpu = jsonlite::unbox(as.character(cpu)),
		memory = jsonlite::unbox(as.character(memory)),
		executionRoleArn = jsonlite::unbox(execution_role),
		taskRoleArn = jsonlite::unbox(task_role)
	))
}
