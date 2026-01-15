import type * as React from "react";
import { twMerge } from "tailwind-merge";

export type PreviewNode = {
	name: string;
	children?: PreviewNode[];
	isRoot?: boolean;
};

type BlueprintPreviewTreeProps = { node: PreviewNode };

export const BlueprintPreviewTree: React.FC<BlueprintPreviewTreeProps> = ({
	node,
}) => {
	return (
		<div className="text-sm text-gray-200">
			<TreeNode node={node} depth={0} />
		</div>
	);
};

type TreeNodeProps = { node: PreviewNode; depth: number };

const TreeNode: React.FC<TreeNodeProps> = ({ node, depth }) => {
	return (
		<div className="mb-1">
			<div className="flex items-center" style={{ paddingLeft: depth * 12 }}>
				<span className="mr-1 text-gray-500">◦</span>
				<span className={twMerge(node.isRoot && "text-blue-400")}>
					{node.name}
				</span>
			</div>

			{node.children?.map((child) => (
				<TreeNode key={child.name} node={child} depth={depth + 1} />
			))}
		</div>
	);
};
